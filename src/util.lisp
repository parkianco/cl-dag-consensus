;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; util.lisp - Utility functions for cl-dag-consensus
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-dag-consensus)

;;; ============================================================================
;;; Constants
;;; ============================================================================

(defconstant +max-parents+ 64
  "Maximum number of parent references per block.
   Limits DAG width to maintain manageable merge complexity.")

(defconstant +min-parents+ 1
  "Minimum number of parent references per block.
   Every block (except genesis) must reference at least one parent.")

(defconstant +default-k-param+ 18
  "Default GHOSTDAG k parameter for blue set selection.
   Controls anticone size tolerance and affects ordering stability.")

(defconstant +merge-threshold+ 3
  "Minimum number of divergent branches to trigger merge block requirement.
   Below this threshold, normal multi-parent blocks suffice.")

(defconstant +finality-threshold+ 100
  "Number of confirming vertices needed for probabilistic finality.
   Higher values provide stronger finality guarantees.")

(defconstant +max-blue-score+ (expt 2 64)
  "Maximum blue score value for overflow prevention.")

(defconstant +orphan-timeout+ 300
  "Seconds before orphan vertices are eligible for pruning.")

;;; ============================================================================
;;; Type Definitions
;;; ============================================================================

(deftype vertex-id ()
  "Unique identifier for a DAG vertex (typically SHA-256 hash)."
  '(simple-array (unsigned-byte 8) (32)))

(deftype vertex-color ()
  "GHOSTDAG vertex coloring: blue (in main DAG), red (out of anticone)."
  '(member :blue :red :uncolored))

(deftype edge-type ()
  "Type of edge relationship between vertices."
  '(member :parent :reference :weak :merge))

(deftype block-hash ()
  "256-bit block hash."
  '(simple-array (unsigned-byte 8) (32)))

;;; ============================================================================
;;; Hash Functions
;;; ============================================================================

(defun zero-hash ()
  "Return a zero-filled 32-byte hash."
  (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))

(defun hash-equal-p (h1 h2)
  "Compare two hashes for equality."
  (and (= (length h1) (length h2))
       (loop for b1 across h1
             for b2 across h2
             always (= b1 b2))))

(defun copy-hash (hash)
  "Create a copy of a hash array."
  (let ((new-hash (make-array 32 :element-type '(unsigned-byte 8))))
    (replace new-hash hash)
    new-hash))

(defun hash-to-hex (hash)
  "Convert hash to hexadecimal string."
  (with-output-to-string (s)
    (loop for byte across hash
          do (format s "~2,'0x" byte))))

(defun hex-to-hash (hex-string)
  "Convert hexadecimal string to hash."
  (let ((hash (make-array 32 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 32
          for pos = (* i 2)
          do (setf (aref hash i)
                   (parse-integer hex-string :start pos :end (+ pos 2) :radix 16)))
    hash))

(defun hash-less-p (h1 h2)
  "Compare two hashes lexicographically."
  (loop for b1 across h1
        for b2 across h2
        when (< b1 b2) return t
        when (> b1 b2) return nil
        finally (return nil)))

;;; ============================================================================
;;; Event Hooks
;;; ============================================================================

(defvar *on-vertex-added* nil
  "Function called when a vertex is added to the DAG.
   Signature: (funcall *on-vertex-added* vertex dag)")

(defvar *on-tip-changed* nil
  "Function called when the set of tips changes.
   Signature: (funcall *on-tip-changed* old-tips new-tips dag)")

(defvar *on-merge-detected* nil
  "Function called when a merge block is detected.
   Signature: (funcall *on-merge-detected* merge-vertex branches dag)")

(defvar *on-finality-reached* nil
  "Function called when a vertex reaches finality.
   Signature: (funcall *on-finality-reached* vertex finality-depth dag)")
