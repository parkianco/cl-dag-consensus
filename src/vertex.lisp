;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; vertex.lisp - Vertex and block types for cl-dag-consensus
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-dag-consensus)

;;; ============================================================================
;;; DAG Vertex Structure
;;; ============================================================================

(defstruct (dag-vertex
            (:constructor %make-dag-vertex)
            (:copier nil)
            (:print-function print-dag-vertex))
  "A vertex in the DAG representing a block with graph metadata.

   Each vertex wraps a block and maintains:
   - Graph connectivity (parents/children lists)
   - Weight metrics for tip selection
   - GHOSTDAG coloring and scoring
   - Merge block indicator"

  ;; Identity
  (id (zero-hash) :type (simple-array (unsigned-byte 8) (*)))
  (hash (zero-hash) :type (simple-array (unsigned-byte 8) (*)))

  ;; Position
  (height 0 :type (unsigned-byte 64))
  (timestamp 0 :type (unsigned-byte 64))

  ;; Work and weight
  (work 0 :type (unsigned-byte 256))
  (cumulative-work 0 :type (unsigned-byte 256))

  ;; Connectivity
  (parents nil :type list)
  (children nil :type list)

  ;; Block data
  (data nil :type t)

  ;; GHOSTDAG properties
  (color :uncolored :type vertex-color)
  (score 0 :type (unsigned-byte 64))
  (selected-parent nil :type (or null (simple-array (unsigned-byte 8) (*))))

  ;; Merge status
  (is-merge-p nil :type boolean)

  ;; Validity cache
  (valid-p t :type boolean)
  (validation-time 0 :type (unsigned-byte 64)))

(defun make-dag-vertex (&key
                         (id (zero-hash))
                         (hash (zero-hash))
                         (height 0)
                         (timestamp 0)
                         (work 0)
                         (parents nil)
                         (data nil)
                         (is-merge-p nil))
  "Create a new DAG vertex with the specified properties."
  (let ((vertex (%make-dag-vertex
                 :id (copy-hash id)
                 :hash (copy-hash hash)
                 :height height
                 :timestamp timestamp
                 :work work
                 :cumulative-work 0
                 :parents (copy-list parents)
                 :children nil
                 :data data
                 :color :uncolored
                 :score 0
                 :is-merge-p is-merge-p)))
    (let ((parent-count (length parents)))
      (when (and (plusp parent-count)
                 (> parent-count +max-parents+))
        (error "DAG vertex has ~D parents, exceeding maximum ~D"
               parent-count +max-parents+)))
    vertex))

(defun print-dag-vertex (vertex stream depth)
  "Print a DAG vertex in a readable format."
  (declare (ignore depth))
  (format stream "#<DAG-VERTEX ~A h=~D parents=~D color=~A score=~D~A>"
          (subseq (hash-to-hex (dag-vertex-hash vertex)) 0 8)
          (dag-vertex-height vertex)
          (length (dag-vertex-parents vertex))
          (dag-vertex-color vertex)
          (dag-vertex-score vertex)
          (if (dag-vertex-is-merge-p vertex) " MERGE" "")))

;;; ============================================================================
;;; DAG Edge Structure
;;; ============================================================================

(defstruct (dag-edge
            (:constructor %make-dag-edge)
            (:copier nil)
            (:print-function print-dag-edge))
  "A directed edge in the DAG from child to parent."

  (from (zero-hash) :type (simple-array (unsigned-byte 8) (*)))
  (to (zero-hash) :type (simple-array (unsigned-byte 8) (*)))
  (weight 1 :type (unsigned-byte 64))
  (type :parent :type edge-type)
  (timestamp 0 :type (unsigned-byte 64)))

(defun make-dag-edge (&key
                       (from (zero-hash))
                       (to (zero-hash))
                       (weight 1)
                       (type :parent)
                       (timestamp 0))
  "Create a new DAG edge from child to parent."
  (%make-dag-edge
   :from (copy-hash from)
   :to (copy-hash to)
   :weight weight
   :type type
   :timestamp timestamp))

(defun print-dag-edge (edge stream depth)
  "Print a DAG edge in a readable format."
  (declare (ignore depth))
  (format stream "#<DAG-EDGE ~A -> ~A type=~A weight=~D>"
          (subseq (hash-to-hex (dag-edge-from edge)) 0 8)
          (subseq (hash-to-hex (dag-edge-to edge)) 0 8)
          (dag-edge-type edge)
          (dag-edge-weight edge)))

;;; ============================================================================
;;; DAG Block Structure
;;; ============================================================================

(defstruct (dag-block
            (:constructor %make-dag-block)
            (:copier nil)
            (:print-function print-dag-block))
  "Extended block structure for DAG consensus with multiple parents."

  ;; Header fields
  (version 1 :type (unsigned-byte 32))
  (parent-hashes nil :type list)
  (merkle-root (zero-hash) :type (simple-array (unsigned-byte 8) (*)))
  (timestamp 0 :type (unsigned-byte 64))
  (bits #x1d00ffff :type (unsigned-byte 32))
  (nonce 0 :type (unsigned-byte 32))
  (blue-score 0 :type (unsigned-byte 64))

  ;; Extended fields
  (hash-cache nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (selected-parent-hash nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (accepted-id-merkle-root nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (utxo-commitment nil :type (or null (simple-array (unsigned-byte 8) (*))))

  ;; Body
  (transactions nil :type list)
  (merge-proof nil :type (or null merge-proof))

  ;; DAG metadata
  (pruning-point nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (daa-score 0 :type (unsigned-byte 64))
  (blue-work 0 :type (unsigned-byte 256)))

(defun make-dag-block (&key
                        (version 1)
                        (parent-hashes nil)
                        (merkle-root (zero-hash))
                        (timestamp 0)
                        (bits #x1d00ffff)
                        (nonce 0)
                        (transactions nil)
                        (merge-proof nil))
  "Create a new DAG block with the specified properties."
  (let ((block (%make-dag-block
                :version version
                :parent-hashes (mapcar #'copy-hash parent-hashes)
                :merkle-root (copy-hash merkle-root)
                :timestamp timestamp
                :bits bits
                :nonce nonce
                :transactions (copy-list transactions)
                :merge-proof merge-proof)))
    (dolist (hash parent-hashes)
      (unless (and (typep hash '(simple-array (unsigned-byte 8) (*)))
                   (= (length hash) 32))
        (error "Invalid parent hash: must be 32-byte array")))
    block))

(defun print-dag-block (block stream depth)
  "Print a DAG block in a readable format."
  (declare (ignore depth))
  (format stream "#<DAG-BLOCK v~D parents=~D txs=~D blue=~D~A>"
          (dag-block-version block)
          (length (dag-block-parent-hashes block))
          (length (dag-block-transactions block))
          (dag-block-blue-score block)
          (if (dag-block-merge-proof block) " MERGE" "")))

(defun dag-block-header (block)
  "Extract header information from DAG block."
  (list (cons :version (dag-block-version block))
        (cons :parent-hashes (dag-block-parent-hashes block))
        (cons :merkle-root (dag-block-merkle-root block))
        (cons :timestamp (dag-block-timestamp block))
        (cons :bits (dag-block-bits block))
        (cons :nonce (dag-block-nonce block))
        (cons :blue-score (dag-block-blue-score block))))

(defun dag-block-hash (block)
  "Compute or return cached hash for a DAG block."
  (or (dag-block-hash-cache block)
      (let ((hash (compute-dag-block-hash block)))
        (setf (dag-block-hash-cache block) hash)
        hash)))

(defun compute-dag-block-hash (block)
  "Compute hash of DAG block header.
   Placeholder - real implementation would use SHA-256."
  (declare (ignore block))
  (zero-hash))

(defun is-genesis-block-p (block)
  "Check if block is the genesis block (no parents)."
  (null (dag-block-parent-hashes block)))

(defun parent-count (block-or-vertex)
  "Return the number of parents for a block or vertex."
  (etypecase block-or-vertex
    (dag-block (length (dag-block-parent-hashes block-or-vertex)))
    (dag-vertex (length (dag-vertex-parents block-or-vertex)))))

(defun is-multi-parent-p (block-or-vertex)
  "Check if block/vertex has multiple parents."
  (> (parent-count block-or-vertex) 1))

;;; ============================================================================
;;; Merge Proof Structure
;;; ============================================================================

(defstruct (merge-proof
            (:constructor %make-merge-proof)
            (:copier nil)
            (:print-function print-merge-proof))
  "Cryptographic proof that a merge block correctly consolidates branches."

  (branches nil :type list)
  (resolution nil :type list)
  (conflict-set nil :type list)
  (ancestor-hash nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (work-proofs nil :type list)
  (timestamp 0 :type (unsigned-byte 64))
  (signature nil :type (or null (simple-array (unsigned-byte 8) (*)))))

(defun make-merge-proof (&key
                          (branches nil)
                          (resolution nil)
                          (conflict-set nil)
                          (ancestor-hash nil)
                          (work-proofs nil)
                          (timestamp 0))
  "Create a new merge proof for a merge block."
  (%make-merge-proof
   :branches (mapcar #'copy-hash branches)
   :resolution (copy-list resolution)
   :conflict-set (copy-list conflict-set)
   :ancestor-hash (when ancestor-hash (copy-hash ancestor-hash))
   :work-proofs (copy-list work-proofs)
   :timestamp timestamp))

(defun print-merge-proof (proof stream depth)
  "Print a merge proof in a readable format."
  (declare (ignore depth))
  (format stream "#<MERGE-PROOF branches=~D conflicts=~D resolutions=~D>"
          (length (merge-proof-branches proof))
          (length (merge-proof-conflict-set proof))
          (length (merge-proof-resolution proof))))

;;; ============================================================================
;;; Conflict Resolution Structure
;;; ============================================================================

(defstruct (conflict-resolution
            (:constructor make-conflict-resolution))
  "Resolution decision for a transaction conflict."

  (conflict-id 0 :type (unsigned-byte 64))
  (tx-accepted nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (tx-rejected nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (reason :heaviest-branch :type keyword)
  (branch-hash nil :type (or null (simple-array (unsigned-byte 8) (*)))))

;;; ============================================================================
;;; Branch Information
;;; ============================================================================

(defstruct (branch-info
            (:constructor make-branch-info))
  "Information about a branch in the DAG for merge analysis."

  (tip-hash (zero-hash) :type (simple-array (unsigned-byte 8) (*)))
  (ancestor-hash (zero-hash) :type (simple-array (unsigned-byte 8) (*)))
  (height 0 :type (unsigned-byte 64))
  (cumulative-work 0 :type (unsigned-byte 256))
  (block-count 0 :type (unsigned-byte 64))
  (transaction-count 0 :type (unsigned-byte 64))
  (first-divergence-time 0 :type (unsigned-byte 64)))

;;; ============================================================================
;;; Validation Result Types
;;; ============================================================================

(defstruct (dag-validation-result
            (:constructor make-dag-validation-result))
  "Result of DAG block or structure validation."

  (valid-p t :type boolean)
  (errors nil :type list)
  (warnings nil :type list)
  (validation-time 0 :type (unsigned-byte 64))
  (checked-properties nil :type list))

;;; ============================================================================
;;; GHOSTDAG Data
;;; ============================================================================

(defstruct (ghostdag-data
            (:constructor make-ghostdag-data))
  "GHOSTDAG ordering data for a vertex."

  (blue-score 0 :type (unsigned-byte 64))
  (blue-work 0 :type (unsigned-byte 256))
  (selected-parent nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (merge-set-blues nil :type list)
  (merge-set-reds nil :type list)
  (blues-anticone-sizes nil :type list))

;;; ============================================================================
;;; Utility Functions for Types
;;; ============================================================================

(defun vertex-id-equal-p (id1 id2)
  "Check if two vertex IDs are equal."
  (hash-equal-p id1 id2))

(defun max-height-parent (vertex vertices-table)
  "Find the parent with maximum height."
  (let ((max-height 0)
        (selected nil))
    (dolist (parent-id (dag-vertex-parents vertex))
      (let ((parent (gethash parent-id vertices-table)))
        (when (and parent (> (dag-vertex-height parent) max-height))
          (setf max-height (dag-vertex-height parent)
                selected parent-id))))
    selected))

;;; ============================================================================
;;; Type Predicates
;;; ============================================================================

(defun valid-vertex-id-p (id)
  "Check if ID is a valid vertex identifier."
  (and (typep id '(simple-array (unsigned-byte 8) (*)))
       (= (length id) 32)))

(defun valid-dag-block-p (block)
  "Basic structural validation of a DAG block."
  (and (dag-block-p block)
       (>= (dag-block-version block) 1)
       (or (is-genesis-block-p block)
           (and (>= (length (dag-block-parent-hashes block)) +min-parents+)
                (<= (length (dag-block-parent-hashes block)) +max-parents+)))
       (= (length (dag-block-merkle-root block)) 32)))

(defun valid-merge-proof-p (proof)
  "Check if merge proof has valid structure."
  (and (merge-proof-p proof)
       (>= (length (merge-proof-branches proof)) 2)
       (every #'valid-vertex-id-p (merge-proof-branches proof))))
