;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; ordering.lisp - Topological ordering and GHOSTDAG for cl-dag-consensus
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-dag-consensus)

;;; ============================================================================
;;; Topological Ordering
;;; ============================================================================

(defun topological-sort (dag)
  "Compute topological ordering of DAG vertices using Kahn's algorithm.
   Vertices are ordered such that parents come before children."
  (let ((in-degree (make-hash-table :test 'equalp))
        (queue nil)
        (result nil))
    ;; Calculate in-degrees (number of parents for each vertex)
    (maphash (lambda (id vertex)
               (setf (gethash id in-degree)
                     (length (dag-vertex-parents vertex))))
             (dag-consensus-vertices dag))

    ;; Find vertices with in-degree 0 (genesis and orphans)
    (maphash (lambda (id degree)
               (when (zerop degree)
                 (push id queue)))
             in-degree)

    ;; Process queue
    (loop while queue do
      (let* ((vertex-id (pop queue))
             (vertex (get-vertex dag vertex-id)))
        (when vertex
          (push vertex result)
          ;; Reduce in-degree of children
          (dolist (child-id (dag-vertex-children vertex))
            (decf (gethash child-id in-degree 0))
            (when (zerop (gethash child-id in-degree))
              (push child-id queue))))))

    (nreverse result)))

(defun reverse-topological-sort (dag)
  "Compute reverse topological ordering (children before parents)."
  (nreverse (topological-sort dag)))

(defun compute-block-order (dag)
  "Compute deterministic block ordering for transaction finality.
   Combines topological order with tie-breaking by:
   1. Blue score (higher first)
   2. Timestamp (earlier first)
   3. Hash (lexicographic)"
  (let ((sorted (topological-sort dag)))
    (stable-sort sorted
                 (lambda (v1 v2)
                   (cond
                     ;; Higher blue score first
                     ((> (dag-vertex-score v1) (dag-vertex-score v2)) t)
                     ((< (dag-vertex-score v1) (dag-vertex-score v2)) nil)
                     ;; Earlier timestamp first
                     ((< (dag-vertex-timestamp v1) (dag-vertex-timestamp v2)) t)
                     ((> (dag-vertex-timestamp v1) (dag-vertex-timestamp v2)) nil)
                     ;; Lexicographic hash comparison
                     (t (hash-less-p (dag-vertex-hash v1) (dag-vertex-hash v2))))))))

;;; ============================================================================
;;; Weight Calculation
;;; ============================================================================

(defun calculate-vertex-weight (dag vertex-id)
  "Calculate the weight contribution of a single vertex."
  (let ((vertex (get-vertex dag vertex-id)))
    (if vertex
        (dag-vertex-work vertex)
        0)))

(defun calculate-branch-weight (dag tip-id)
  "Calculate the total weight of a branch from genesis to tip."
  (let ((vertex (get-vertex dag tip-id)))
    (if (null vertex)
        0
        (let ((cached-work (dag-vertex-cumulative-work vertex)))
          (if (plusp cached-work)
              cached-work
              (compute-cumulative-work dag vertex))))))

(defun compute-cumulative-work (dag vertex)
  "Compute cumulative work for a vertex with memoization.
   cumulative_work(v) = work(v) + max(cumulative_work(p) for p in parents(v))"
  (let ((own-work (dag-vertex-work vertex))
        (max-parent-work 0))
    ;; Find maximum parent cumulative work
    (dolist (parent-id (dag-vertex-parents vertex))
      (let ((parent (get-vertex dag parent-id)))
        (when parent
          (let ((parent-work (dag-vertex-cumulative-work parent)))
            (when (zerop parent-work)
              (setf parent-work (compute-cumulative-work dag parent)))
            (when (> parent-work max-parent-work)
              (setf max-parent-work parent-work))))))
    (let ((total-work (+ own-work max-parent-work)))
      ;; Cache the result
      (setf (dag-vertex-cumulative-work vertex) total-work)
      total-work)))

(defun calculate-cumulative-work (dag vertex-id)
  "Public interface to compute cumulative work for a vertex."
  (let ((vertex (get-vertex dag vertex-id)))
    (if vertex
        (compute-cumulative-work dag vertex)
        0)))

(defun update-weights (dag)
  "Recompute all vertex weights in the DAG."
  (let ((count 0))
    (dolist (vertex (topological-sort dag))
      (compute-cumulative-work dag vertex)
      (incf count))
    count))

;;; ============================================================================
;;; Tip Selection
;;; ============================================================================

(defun tip-better-p (v1 v2)
  "Compare two tips, return T if v1 is better than v2."
  (cond
    ;; Higher cumulative work is better
    ((> (dag-vertex-cumulative-work v1)
        (dag-vertex-cumulative-work v2))
     t)
    ((< (dag-vertex-cumulative-work v1)
        (dag-vertex-cumulative-work v2))
     nil)
    ;; Equal work: higher blue score is better
    ((> (dag-vertex-score v1) (dag-vertex-score v2))
     t)
    ((< (dag-vertex-score v1) (dag-vertex-score v2))
     nil)
    ;; Equal score: lower hash is better (deterministic)
    (t (hash-less-p (dag-vertex-hash v1) (dag-vertex-hash v2)))))

(defun select-best-tip (dag)
  "Select the single best tip based on heaviest branch rule."
  (let ((tips (get-tips dag)))
    (when tips
      (first (sort (copy-list tips)
                   (lambda (v1 v2)
                     (tip-better-p v1 v2)))))))

(defun select-best-tips (dag count)
  "Select the N best tips for multi-parent block creation."
  (let ((tips (get-tips dag)))
    (when tips
      (let ((sorted-tips (sort (copy-list tips)
                               (lambda (v1 v2)
                                 (tip-better-p v1 v2)))))
        (subseq sorted-tips 0 (min count (length sorted-tips)))))))

(defun rank-tips-by-weight (dag)
  "Rank all tips by their branch weight."
  (let ((tips (get-tips dag)))
    (sort (mapcar (lambda (tip)
                    (cons tip (dag-vertex-cumulative-work tip)))
                  tips)
          #'>
          :key #'cdr)))

(defun select-balanced-tips (dag count)
  "Select tips balancing weight and DAG coverage."
  (let* ((tips (get-tips dag))
         (selected nil)
         (covered (make-hash-table :test 'equalp)))
    ;; Start with the heaviest tip
    (when tips
      (let ((best (first (sort (copy-list tips)
                               (lambda (v1 v2)
                                 (> (dag-vertex-cumulative-work v1)
                                    (dag-vertex-cumulative-work v2)))))))
        (push best selected)
        ;; Mark ancestors as covered
        (dolist (ancestor (get-ancestors dag (dag-vertex-id best) :max-depth 50))
          (setf (gethash (dag-vertex-id ancestor) covered) t))))

    ;; Add tips that cover new areas
    (dolist (tip (remove (first selected) tips))
      (when (>= (length selected) count)
        (return))
      (let ((new-ancestors 0))
        ;; Count uncovered ancestors
        (dolist (ancestor (get-ancestors dag (dag-vertex-id tip) :max-depth 50))
          (unless (gethash (dag-vertex-id ancestor) covered)
            (incf new-ancestors)))
        ;; Select if covers significant new area
        (when (> new-ancestors 5)
          (push tip selected)
          (dolist (ancestor (get-ancestors dag (dag-vertex-id tip) :max-depth 50))
            (setf (gethash (dag-vertex-id ancestor) covered) t)))))

    (nreverse selected)))

(defun tip-selection-strategy (dag strategy &key (count 2))
  "Select tips using the specified strategy."
  (case strategy
    (:heaviest
     (select-best-tips dag count))
    (:freshest
     (let ((tips (get-tips dag)))
       (subseq (sort (copy-list tips) #'>
                     :key #'dag-vertex-timestamp)
               0 (min count (length tips)))))
    (:balanced
     (select-balanced-tips dag count))
    (otherwise
     (select-best-tips dag count))))

;;; ============================================================================
;;; Branch Analysis
;;; ============================================================================

(defun find-best-parent (dag vertex)
  "Find the parent with highest cumulative work."
  (let ((best-parent nil)
        (best-work 0))
    (dolist (parent-id (dag-vertex-parents vertex))
      (let ((parent (get-vertex dag parent-id)))
        (when (and parent
                   (> (dag-vertex-cumulative-work parent) best-work))
          (setf best-parent parent
                best-work (dag-vertex-cumulative-work parent)))))
    best-parent))

(defun find-heaviest-branch (dag)
  "Find the heaviest branch in the DAG."
  (let ((best-tip (select-best-tip dag)))
    (when best-tip
      (let ((branch nil)
            (current best-tip))
        ;; Traverse from tip to genesis following selected parents
        (loop while current do
          (push current branch)
          (setf current
                (when (dag-vertex-parents current)
                  (find-best-parent dag current))))
        branch))))

(defun compare-branches (dag tip1-id tip2-id)
  "Compare two branches and determine which is heavier.
   Returns: -1 if tip1 heavier, 0 if equal, 1 if tip2 heavier"
  (let ((work1 (calculate-branch-weight dag tip1-id))
        (work2 (calculate-branch-weight dag tip2-id)))
    (cond
      ((> work1 work2) -1)
      ((< work1 work2) 1)
      (t 0))))

(defun branch-work-ratio (dag tip1-id tip2-id)
  "Calculate the work ratio between two branches."
  (let ((work1 (calculate-branch-weight dag tip1-id))
        (work2 (calculate-branch-weight dag tip2-id)))
    (if (zerop work2)
        (if (zerop work1) 1.0 most-positive-single-float)
        (/ (float work1) work2))))

(defun branch-depth (dag tip-id)
  "Calculate the depth (height) of a branch."
  (let ((vertex (get-vertex dag tip-id)))
    (if vertex
        (dag-vertex-height vertex)
        0)))

;;; ============================================================================
;;; GHOSTDAG Implementation
;;; ============================================================================

(defun get-anticone (dag vertex-id)
  "Compute the anticone of a vertex (incomparable vertices)."
  (let ((ancestors (make-hash-table :test 'equalp))
        (descendants (make-hash-table :test 'equalp))
        (anticone nil))
    ;; Mark ancestors
    (dolist (ancestor (get-ancestors dag vertex-id))
      (setf (gethash (dag-vertex-id ancestor) ancestors) t))
    ;; Mark descendants
    (dolist (descendant (get-descendants dag vertex-id))
      (setf (gethash (dag-vertex-id descendant) descendants) t))
    ;; Collect anticone (vertices that are neither)
    (maphash (lambda (id vertex)
               (unless (or (hash-equal-p id vertex-id)
                           (gethash id ancestors)
                           (gethash id descendants))
                 (push vertex anticone)))
             (dag-consensus-vertices dag))
    anticone))

(defun find-selected-parent (dag vertex)
  "Find the selected parent for GHOSTDAG ordering."
  (let ((best-parent nil)
        (best-score -1))
    (dolist (parent-id (dag-vertex-parents vertex))
      (let ((parent (get-vertex dag parent-id)))
        (when (and parent (> (dag-vertex-score parent) best-score))
          (setf best-parent parent
                best-score (dag-vertex-score parent)))))
    best-parent))

(defun compute-merge-set (dag vertex selected-parent)
  "Compute the merge set for a vertex.
   Merge set = past(vertex) - past(selected_parent) - {selected_parent}"
  (when (null selected-parent)
    (return-from compute-merge-set nil))

  (let ((vertex-past (make-hash-table :test 'equalp))
        (parent-past (make-hash-table :test 'equalp))
        (merge-set nil))
    ;; Collect vertex's past
    (dolist (ancestor (get-ancestors dag (dag-vertex-id vertex)))
      (setf (gethash (dag-vertex-id ancestor) vertex-past) ancestor))
    ;; Collect selected parent's past
    (setf (gethash (dag-vertex-id selected-parent) parent-past) t)
    (dolist (ancestor (get-ancestors dag (dag-vertex-id selected-parent)))
      (setf (gethash (dag-vertex-id ancestor) parent-past) t))
    ;; Merge set = vertex-past - parent-past
    (maphash (lambda (id vertex)
               (unless (gethash id parent-past)
                 (push vertex merge-set)))
             vertex-past)
    merge-set))

(defun calculate-blue-score (dag vertex-id)
  "Calculate the blue score for a vertex using GHOSTDAG.
   Blue score = blue_score(selected_parent) + 1 + |blues in merge set|"
  (let ((vertex (get-vertex dag vertex-id)))
    (when vertex
      (let ((selected-parent (find-selected-parent dag vertex))
            (blues-in-merge 0))
        ;; Base case: genesis has blue score 0
        (when (null (dag-vertex-parents vertex))
          (setf (dag-vertex-score vertex) 0)
          (setf (dag-vertex-color vertex) :blue)
          (return-from calculate-blue-score 0))

        ;; Get parent's blue score
        (let ((parent-score (if selected-parent
                                (dag-vertex-score selected-parent)
                                0)))
          ;; Count blue blocks in the merge set
          (dolist (merge-vertex (compute-merge-set dag vertex selected-parent))
            (when (eq (dag-vertex-color merge-vertex) :blue)
              (incf blues-in-merge)))

          (let ((score (+ parent-score 1 blues-in-merge)))
            (setf (dag-vertex-score vertex) score)
            score))))))

(defun get-blue-set (dag vertex-id)
  "Get the blue set for a vertex (ordered blue ancestors)."
  (remove-if-not (lambda (v)
                   (eq (dag-vertex-color v) :blue))
                 (get-ancestors dag vertex-id)))

(defun get-red-set (dag vertex-id)
  "Get the red set for a vertex (non-blue ancestors)."
  (remove-if (lambda (v)
               (eq (dag-vertex-color v) :blue))
             (get-ancestors dag vertex-id)))

(defun color-vertex (dag vertex-id k)
  "Assign GHOSTDAG coloring to a vertex."
  (let* ((vertex (get-vertex dag vertex-id))
         (selected-parent (find-selected-parent dag vertex)))
    (cond
      ;; Genesis is always blue
      ((null (dag-vertex-parents vertex))
       (setf (dag-vertex-color vertex) :blue)
       :blue)
      ;; No selected parent (shouldn't happen)
      ((null selected-parent)
       (setf (dag-vertex-color vertex) :red)
       :red)
      ;; Check anticone size
      (t
       (let ((anticone-count 0)
             (parent-blues (get-blue-set dag (dag-vertex-id selected-parent))))
         ;; Count anticone intersection with parent blues
         (dolist (blue-ancestor parent-blues)
           (let ((ancestor-id (dag-vertex-id blue-ancestor)))
             (unless (or (is-ancestor-p dag vertex-id ancestor-id)
                         (is-ancestor-p dag ancestor-id vertex-id))
               (incf anticone-count))))
         (if (<= anticone-count k)
             (progn
               (setf (dag-vertex-color vertex) :blue)
               :blue)
             (progn
               (setf (dag-vertex-color vertex) :red)
               :red)))))))

(defun ghostdag-ordering (dag)
  "Compute the complete GHOSTDAG ordering of the DAG."
  (let ((k (dag-consensus-k-param dag)))
    ;; Color all vertices in topological order
    (dolist (vertex (topological-sort dag))
      (color-vertex dag (dag-vertex-id vertex) k)
      (calculate-blue-score dag (dag-vertex-id vertex)))
    ;; Sort by blue score, then by hash for ties
    (sort (get-vertices dag)
          (lambda (v1 v2)
            (cond
              ((< (dag-vertex-score v1) (dag-vertex-score v2)) t)
              ((> (dag-vertex-score v1) (dag-vertex-score v2)) nil)
              (t (hash-less-p (dag-vertex-hash v1) (dag-vertex-hash v2))))))))

;;; ============================================================================
;;; Transaction Ordering
;;; ============================================================================

(defun transaction-total-order (dag)
  "Compute total ordering of transactions across the DAG."
  (let ((ordering nil))
    (dolist (vertex (ghostdag-ordering dag))
      (let ((block (dag-vertex-data vertex)))
        (when (and block (dag-block-p block))
          (loop for tx in (dag-block-transactions block)
                for i from 0
                do (push (cons vertex i) ordering)))))
    (nreverse ordering)))

;;; ============================================================================
;;; Finality Computation
;;; ============================================================================

(defun compute-finality-score (dag vertex-id)
  "Compute the finality score for a vertex."
  (let ((vertex (get-vertex dag vertex-id))
        (descendants (get-descendants dag vertex-id)))
    (when vertex
      (let ((descendant-count (length descendants))
            (descendant-work 0))
        (dolist (desc descendants)
          (incf descendant-work (dag-vertex-work desc)))
        (+ descendant-count (floor descendant-work 1000000))))))

(defun is-final-p (dag vertex-id)
  "Check if a vertex has reached finality."
  (let ((descendants (get-descendants dag vertex-id)))
    (or (>= (length descendants) +finality-threshold+)
        (>= (compute-finality-score dag vertex-id)
            (dag-consensus-finality-window dag)))))

(defun finality-depth (dag vertex-id)
  "Calculate the depth of finality for a vertex."
  (length (get-descendants dag vertex-id)))

(defun confirmation-count (dag vertex-id)
  "Get the number of confirmations for a vertex."
  (finality-depth dag vertex-id))

;;; ============================================================================
;;; Common Ancestor
;;; ============================================================================

(defun find-common-ancestor (dag id1 id2)
  "Find the most recent common ancestor of two vertices."
  (let ((ancestors1 (make-hash-table :test 'equalp)))
    ;; Collect ancestors of id1
    (dolist (ancestor (get-ancestors dag id1))
      (setf (gethash (dag-vertex-id ancestor) ancestors1) ancestor))
    ;; Find highest common ancestor
    (let ((best-ancestor nil)
          (best-height 0))
      (dolist (ancestor (get-ancestors dag id2))
        (let ((id (dag-vertex-id ancestor)))
          (when (and (gethash id ancestors1)
                     (> (dag-vertex-height ancestor) best-height))
            (setf best-ancestor id
                  best-height (dag-vertex-height ancestor)))))
      best-ancestor)))
