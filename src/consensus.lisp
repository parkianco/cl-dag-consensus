;;;; consensus.lisp - Merge block operations and finality for cl-dag-consensus
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-dag-consensus)

;;; ============================================================================
;;; Merge Block Detection
;;; ============================================================================

(defun detect-merge-candidates (dag)
  "Detect situations where a merge block should be created."
  (let ((tips (get-tips dag))
        (candidates nil))
    (when (>= (length tips) +merge-threshold+)
      ;; Group tips by divergence
      (let ((groups (group-divergent-tips dag tips)))
        (dolist (group groups)
          (when (>= (length group) 2)
            (push group candidates)))))
    candidates))

(defun group-divergent-tips (dag tips)
  "Group tips based on their divergence from each other."
  (let ((groups nil)
        (assigned (make-hash-table :test 'equalp)))
    (dolist (tip tips)
      (unless (gethash (dag-vertex-id tip) assigned)
        (let ((group (list tip)))
          (setf (gethash (dag-vertex-id tip) assigned) t)
          ;; Find other tips that should merge with this one
          (dolist (other-tip tips)
            (unless (gethash (dag-vertex-id other-tip) assigned)
              (when (should-merge-tips-p dag tip other-tip)
                (push other-tip group)
                (setf (gethash (dag-vertex-id other-tip) assigned) t))))
          (push (nreverse group) groups))))
    (nreverse groups)))

(defun should-merge-tips-p (dag tip1 tip2)
  "Determine if two tips should be merged."
  (let ((common-ancestor-id (find-common-ancestor dag
                                                   (dag-vertex-id tip1)
                                                   (dag-vertex-id tip2))))
    (when common-ancestor-id
      (let ((common-ancestor (get-vertex dag common-ancestor-id)))
        (and common-ancestor
             ;; Significant divergence (at least 3 blocks each)
             (>= (- (dag-vertex-height tip1)
                    (dag-vertex-height common-ancestor))
                 3)
             (>= (- (dag-vertex-height tip2)
                    (dag-vertex-height common-ancestor))
                 3))))))

(defun is-merge-block-p (dag vertex-id)
  "Check if a block is a merge block."
  (let ((vertex (get-vertex dag vertex-id)))
    (and vertex
         (dag-vertex-is-merge-p vertex)
         (> (length (dag-vertex-parents vertex)) 1)
         (let ((block (dag-vertex-data vertex)))
           (and block
                (dag-block-p block)
                (dag-block-merge-proof block))))))

(defun find-divergence-point (dag tip1-id tip2-id)
  "Find the point where two branches diverged."
  (let ((common-id (find-common-ancestor dag tip1-id tip2-id)))
    (when common-id
      (get-vertex dag common-id))))

(defun find-convergence-points (dag)
  "Find all points where branches have converged (merge blocks)."
  (let ((merge-blocks nil))
    (maphash (lambda (id vertex)
               (declare (ignore id))
               (when (dag-vertex-is-merge-p vertex)
                 (push vertex merge-blocks)))
             (dag-consensus-vertices dag))
    merge-blocks))

;;; ============================================================================
;;; Merge Block Validation
;;; ============================================================================

(defun validate-merge-block (dag vertex)
  "Validate a merge block for correctness."
  (let ((errors nil)
        (warnings nil)
        (block (dag-vertex-data vertex)))

    ;; Check basic structure
    (unless (dag-vertex-is-merge-p vertex)
      (push "Vertex is not marked as merge block" errors))

    (unless block
      (push "Merge vertex has no block data" errors)
      (return-from validate-merge-block
        (make-dag-validation-result
         :valid-p nil
         :errors errors)))

    (unless (dag-block-p block)
      (push "Block data is not a dag-block" errors)
      (return-from validate-merge-block
        (make-dag-validation-result
         :valid-p nil
         :errors errors)))

    ;; Check parent count
    (let ((parent-count (length (dag-vertex-parents vertex))))
      (when (< parent-count 2)
        (push "Merge block must have at least 2 parents" errors))
      (when (> parent-count +max-parents+)
        (push (format nil "Too many parents: ~D > ~D" parent-count +max-parents+)
              errors)))

    ;; Validate all parents exist
    (dolist (parent-id (dag-vertex-parents vertex))
      (unless (get-vertex dag parent-id)
        (push (format nil "Missing parent: ~A" (hash-to-hex parent-id))
              errors)))

    ;; Check parents are from divergent branches
    (unless (parents-divergent-p dag vertex)
      (push "Merge block parents are not from divergent branches" warnings))

    ;; Validate merge proof
    (let ((proof (dag-block-merge-proof block)))
      (if proof
          (let ((proof-result (validate-merge-proof dag proof vertex)))
            (unless (dag-validation-result-valid-p proof-result)
              (setf errors (append errors (dag-validation-result-errors proof-result))))
            (setf warnings (append warnings (dag-validation-result-warnings proof-result))))
          (push "Merge block is missing merge proof" errors)))

    ;; Check for double-included transactions
    (let ((tx-conflicts (check-transaction-conflicts dag vertex)))
      (dolist (conflict tx-conflicts)
        (push (format nil "Transaction conflict: ~A" conflict) errors)))

    (make-dag-validation-result
     :valid-p (null errors)
     :errors (nreverse errors)
     :warnings (nreverse warnings)
     :checked-properties '(:parent-existence :parent-divergence
                           :merge-proof :transaction-conflicts))))

(defun parents-divergent-p (dag vertex)
  "Check if the parents of a vertex are from divergent branches."
  (let ((parent-ids (dag-vertex-parents vertex)))
    (loop for i from 0 below (length parent-ids)
          for id1 = (nth i parent-ids)
          never (loop for j from 0 below (length parent-ids)
                      for id2 = (nth j parent-ids)
                      when (and (/= i j)
                                (is-ancestor-p dag id1 id2))
                        return t))))

(defun validate-merge-proof (dag proof vertex)
  "Validate a merge proof for correctness."
  (let ((errors nil)
        (warnings nil))

    ;; Check branches match parents
    (let ((proof-branches (merge-proof-branches proof))
          (parent-hashes (dag-vertex-parents vertex)))
      (unless (= (length proof-branches) (length parent-hashes))
        (push "Proof branches count doesn't match parent count" errors))
      (dolist (branch proof-branches)
        (unless (member branch parent-hashes :test #'hash-equal-p)
          (push (format nil "Proof branch ~A not in parents"
                        (hash-to-hex branch))
                errors))))

    ;; Validate conflict resolutions
    (dolist (resolution (merge-proof-resolution proof))
      (unless (valid-resolution-p dag resolution)
        (push (format nil "Invalid resolution: ~A"
                      (conflict-resolution-conflict-id resolution))
              errors)))

    ;; Check ancestor hash
    (when (merge-proof-ancestor-hash proof)
      (unless (get-vertex dag (merge-proof-ancestor-hash proof))
        (push "Merge proof ancestor not found in DAG" warnings)))

    (make-dag-validation-result
     :valid-p (null errors)
     :errors (nreverse errors)
     :warnings (nreverse warnings)
     :checked-properties '(:branches :resolutions :ancestor))))

(defun valid-resolution-p (dag resolution)
  "Check if a conflict resolution is valid."
  (declare (ignore dag))
  (and (conflict-resolution-p resolution)
       (conflict-resolution-tx-accepted resolution)
       (conflict-resolution-tx-rejected resolution)
       (member (conflict-resolution-reason resolution)
               '(:heaviest-branch :first-seen :fee-priority))))

(defun check-merge-conflicts (dag vertex)
  "Check for unresolved conflicts in a merge block."
  (let ((conflicts nil)
        (block (dag-vertex-data vertex)))
    (when (and block (dag-block-p block))
      (let ((proof (dag-block-merge-proof block)))
        (when proof
          (let ((resolved (make-hash-table :test 'equalp)))
            ;; Mark resolved conflicts
            (dolist (resolution (merge-proof-resolution proof))
              (setf (gethash (conflict-resolution-conflict-id resolution)
                             resolved)
                    t))
            ;; Check for unresolved
            (dolist (conflict (merge-proof-conflict-set proof))
              (unless (gethash conflict resolved)
                (push conflict conflicts)))))))
    conflicts))

(defun verify-merge-ancestry (dag vertex)
  "Verify the ancestry claimed in a merge block."
  (let ((block (dag-vertex-data vertex)))
    (when (and block (dag-block-p block))
      (let ((proof (dag-block-merge-proof block)))
        (when (and proof (merge-proof-ancestor-hash proof))
          (let ((ancestor-id (merge-proof-ancestor-hash proof)))
            ;; Verify ancestor is reachable from all parents
            (every (lambda (parent-id)
                     (is-ancestor-p dag parent-id ancestor-id))
                   (dag-vertex-parents vertex))))))))

;;; ============================================================================
;;; Merge Block Creation
;;; ============================================================================

(defun create-merge-block (dag parent-ids &key transactions timestamp)
  "Create a new merge block that consolidates multiple branches."
  (let* ((timestamp (or timestamp (get-internal-real-time)))
         (merge-proof (compute-merge-proof dag parent-ids))
         (resolved-txs (resolve-transaction-conflicts
                        dag parent-ids transactions merge-proof))
         (parent-hashes (mapcar (lambda (id)
                                  (let ((v (get-vertex dag id)))
                                    (if v (dag-vertex-hash v) id)))
                                parent-ids)))

    (make-dag-block
     :version 1
     :parent-hashes parent-hashes
     :timestamp timestamp
     :transactions resolved-txs
     :merge-proof merge-proof)))

(defun compute-merge-proof (dag parent-ids)
  "Compute a merge proof for a set of parents."
  (let ((ancestor-id (find-common-ancestor-multi dag parent-ids))
        (conflicts (find-all-conflicts dag parent-ids))
        (work-proofs nil))

    ;; Collect work proofs for each branch
    (dolist (parent-id parent-ids)
      (push (cons parent-id (calculate-branch-weight dag parent-id))
            work-proofs))

    ;; Generate resolutions for conflicts
    (let ((resolutions (generate-conflict-resolutions dag conflicts work-proofs)))
      (make-merge-proof
       :branches parent-ids
       :resolution resolutions
       :conflict-set (mapcar #'car conflicts)
       :ancestor-hash ancestor-id
       :work-proofs (nreverse work-proofs)
       :timestamp (get-internal-real-time)))))

(defun find-common-ancestor-multi (dag vertex-ids)
  "Find the most recent common ancestor of multiple vertices."
  (when (null vertex-ids)
    (return-from find-common-ancestor-multi nil))
  (when (= (length vertex-ids) 1)
    (return-from find-common-ancestor-multi (first vertex-ids)))

  ;; Start with first pair
  (let ((common (find-common-ancestor dag (first vertex-ids) (second vertex-ids))))
    ;; Intersect with remaining
    (dolist (id (cddr vertex-ids))
      (when common
        (setf common (find-common-ancestor dag common id))))
    common))

(defun select-merge-parents (dag &key (count 2) (strategy :heaviest))
  "Select appropriate parents for a merge block."
  (let ((candidates (detect-merge-candidates dag)))
    (when candidates
      ;; Get the first (best) candidate set
      (let ((best-set (first candidates)))
        (case strategy
          (:heaviest
           (mapcar #'dag-vertex-id
                   (subseq (sort best-set
                                 (lambda (v1 v2)
                                   (> (dag-vertex-cumulative-work v1)
                                      (dag-vertex-cumulative-work v2))))
                           0 (min count (length best-set)))))
          (:balanced
           (mapcar #'dag-vertex-id
                   (select-balanced-tips dag count)))
          (otherwise
           (mapcar #'dag-vertex-id
                   (subseq best-set 0 (min count (length best-set))))))))))

(defun resolve-transaction-conflicts (dag parent-ids transactions proof)
  "Resolve transaction conflicts according to the merge proof."
  (declare (ignore parent-ids))
  (let ((rejected (make-hash-table :test 'equalp)))
    ;; Mark rejected transactions
    (dolist (resolution (merge-proof-resolution proof))
      (let ((tx-id (conflict-resolution-tx-rejected resolution)))
        (when tx-id
          (setf (gethash tx-id rejected) t))))
    ;; Filter out rejected transactions
    (remove-if (lambda (tx)
                 (let ((tx-id (get-transaction-id tx)))
                   (gethash tx-id rejected)))
               transactions)))

(defun get-transaction-id (tx)
  "Extract transaction ID from a transaction."
  (if (and tx (listp tx) (getf tx :id))
      (getf tx :id)
      (zero-hash)))

;;; ============================================================================
;;; Conflict Detection and Resolution
;;; ============================================================================

(defun find-conflicting-transactions (dag parent-ids)
  "Find all conflicting transactions between branches."
  (let ((conflicts nil)
        (utxo-spends (make-hash-table :test 'equal)))

    ;; Collect all UTXO spends from each branch
    (dolist (parent-id parent-ids)
      (dolist (ancestor (cons (get-vertex dag parent-id)
                              (get-ancestors dag parent-id)))
        (let ((block (dag-vertex-data ancestor)))
          (when (and block (dag-block-p block))
            (dolist (tx (dag-block-transactions block))
              (dolist (input (get-tx-inputs tx))
                (let ((utxo-key (format nil "~A:~D"
                                        (getf input :txid)
                                        (getf input :vout))))
                  (let ((existing (gethash utxo-key utxo-spends)))
                    (if existing
                        ;; Conflict found!
                        (push (list existing tx :double-spend) conflicts)
                        (setf (gethash utxo-key utxo-spends) tx))))))))))
    conflicts))

(defun find-all-conflicts (dag parent-ids)
  "Find all conflicts between branches."
  (let ((tx-conflicts (find-conflicting-transactions dag parent-ids))
        (all-conflicts nil)
        (conflict-counter 0))
    (dolist (conflict tx-conflicts)
      (push (cons conflict-counter conflict) all-conflicts)
      (incf conflict-counter))
    all-conflicts))

(defun get-tx-inputs (tx)
  "Extract inputs from a transaction."
  (if (and tx (listp tx))
      (getf tx :inputs nil)
      nil))

(defun resolve-double-spends (dag conflicts work-proofs)
  "Resolve double-spend conflicts using the heaviest branch rule."
  (declare (ignore dag))
  (let ((resolutions nil)
        (work-table (make-hash-table :test 'equalp)))

    ;; Build work lookup table
    (dolist (wp work-proofs)
      (setf (gethash (car wp) work-table) (cdr wp)))

    ;; Resolve each conflict
    (dolist (conflict conflicts)
      (destructuring-bind (conflict-id tx1 tx2 conflict-type) conflict
        (declare (ignore conflict-type))
        ;; Determine which branch each tx is from
        (let ((tx1-branch (find-tx-branch work-proofs tx1))
              (tx2-branch (find-tx-branch work-proofs tx2)))
          (let ((work1 (or (gethash tx1-branch work-table) 0))
                (work2 (or (gethash tx2-branch work-table) 0)))
            (if (>= work1 work2)
                (push (make-conflict-resolution
                       :conflict-id conflict-id
                       :tx-accepted (get-transaction-id tx1)
                       :tx-rejected (get-transaction-id tx2)
                       :reason :heaviest-branch
                       :branch-hash tx1-branch)
                      resolutions)
                (push (make-conflict-resolution
                       :conflict-id conflict-id
                       :tx-accepted (get-transaction-id tx2)
                       :tx-rejected (get-transaction-id tx1)
                       :reason :heaviest-branch
                       :branch-hash tx2-branch)
                      resolutions))))))
    resolutions))

(defun find-tx-branch (work-proofs tx)
  "Find which branch a transaction belongs to."
  (declare (ignore tx))
  (when work-proofs
    (caar work-proofs)))

(defun generate-conflict-resolutions (dag conflicts work-proofs)
  "Generate resolutions for all conflicts."
  (resolve-double-spends dag conflicts work-proofs))

(defun determine-canonical-tx (dag tx1-id tx2-id)
  "Determine which of two conflicting transactions is canonical."
  (declare (ignore dag))
  (if (hash-less-p tx1-id tx2-id) tx1-id tx2-id))

(defun conflict-resolution-strategy (dag strategy-name)
  "Get conflict resolution strategy function by name."
  (declare (ignore dag))
  (case strategy-name
    (:heaviest-branch
     (lambda (tx1 tx2 work1 work2)
       (if (>= work1 work2) tx1 tx2)))
    (:first-seen
     (lambda (tx1 tx2 work1 work2)
       (declare (ignore work1 work2))
       (if (< (getf tx1 :timestamp 0) (getf tx2 :timestamp 0))
           tx1 tx2)))
    (:fee-priority
     (lambda (tx1 tx2 work1 work2)
       (declare (ignore work1 work2))
       (if (> (getf tx1 :fee 0) (getf tx2 :fee 0))
           tx1 tx2)))
    (otherwise
     (lambda (tx1 tx2 work1 work2)
       (if (>= work1 work2) tx1 tx2)))))

;;; ============================================================================
;;; Transaction Conflict Validation
;;; ============================================================================

(defun check-transaction-conflicts (dag vertex)
  "Check for transaction conflicts within a merge block."
  (declare (ignore dag))
  (let ((conflicts nil)
        (block (dag-vertex-data vertex)))
    (when (and block (dag-block-p block))
      (let ((utxos-spent (make-hash-table :test 'equal)))
        (dolist (tx (dag-block-transactions block))
          (dolist (input (get-tx-inputs tx))
            (let ((utxo-key (format nil "~A:~D"
                                    (getf input :txid)
                                    (getf input :vout))))
              (if (gethash utxo-key utxos-spent)
                  (push (format nil "Double-spend detected: ~A" utxo-key)
                        conflicts)
                  (setf (gethash utxo-key utxos-spent) tx)))))))
    conflicts))

(defun validate-dag-block (dag block)
  "Validate a DAG block for structural correctness."
  (let ((errors nil)
        (warnings nil))

    ;; Check version
    (unless (>= (dag-block-version block) 1)
      (push "Invalid block version" errors))

    ;; Check parent hashes
    (when (and (not (is-genesis-block-p block))
               (null (dag-block-parent-hashes block)))
      (push "Non-genesis block has no parents" errors))

    (dolist (hash (dag-block-parent-hashes block))
      (unless (= (length hash) 32)
        (push (format nil "Invalid parent hash length: ~D" (length hash))
              errors)))

    ;; Check parent count
    (let ((parent-count (length (dag-block-parent-hashes block))))
      (when (and (not (is-genesis-block-p block))
                 (> parent-count +max-parents+))
        (push (format nil "Too many parents: ~D" parent-count) errors)))

    ;; Validate merkle root length
    (unless (= (length (dag-block-merkle-root block)) 32)
      (push "Invalid merkle root length" errors))

    ;; Check timestamp is not in future
    (let ((current-time (get-internal-real-time)))
      (when (> (dag-block-timestamp block) (+ current-time 7200))
        (push "Block timestamp is too far in the future" warnings)))

    ;; Validate merge proof if present
    (when (dag-block-merge-proof block)
      (let ((vertex (make-dag-vertex
                     :parents (dag-block-parent-hashes block)
                     :data block
                     :is-merge-p t)))
        (let ((proof-result (validate-merge-proof dag
                                                  (dag-block-merge-proof block)
                                                  vertex)))
          (setf errors (append errors (dag-validation-result-errors proof-result)))
          (setf warnings (append warnings (dag-validation-result-warnings proof-result))))))

    (make-dag-validation-result
     :valid-p (null errors)
     :errors (nreverse errors)
     :warnings (nreverse warnings)
     :checked-properties '(:version :parents :merkle-root :timestamp :merge-proof))))

;;; ============================================================================
;;; Merge Statistics
;;; ============================================================================

(defun merge-statistics (dag)
  "Calculate statistics about merge blocks in the DAG."
  (let ((merge-count 0)
        (total-parents 0)
        (max-parents 0)
        (conflicts-resolved 0))

    (maphash (lambda (id vertex)
               (declare (ignore id))
               (when (dag-vertex-is-merge-p vertex)
                 (incf merge-count)
                 (let ((parent-count (length (dag-vertex-parents vertex))))
                   (incf total-parents parent-count)
                   (when (> parent-count max-parents)
                     (setf max-parents parent-count)))
                 (let ((block (dag-vertex-data vertex)))
                   (when (and block (dag-block-p block))
                     (let ((proof (dag-block-merge-proof block)))
                       (when proof
                         (incf conflicts-resolved
                               (length (merge-proof-resolution proof)))))))))
             (dag-consensus-vertices dag))

    (list :merge-block-count merge-count
          :avg-parents-per-merge (if (plusp merge-count)
                                     (/ (float total-parents) merge-count)
                                     0.0)
          :max-parents-in-merge max-parents
          :total-conflicts-resolved conflicts-resolved)))
