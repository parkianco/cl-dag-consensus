;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; dag.lisp - DAG structure and operations for cl-dag-consensus
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-dag-consensus)

;;; ============================================================================
;;; DAG Consensus Structure
;;; ============================================================================

(defstruct (dag-consensus
            (:constructor %make-dag-consensus)
            (:copier nil)
            (:print-function print-dag-consensus))
  "Main DAG consensus data structure."

  ;; Core graph data
  (genesis nil :type (or null dag-vertex))
  (vertices (make-hash-table :test 'equalp) :type hash-table)
  (edges (make-hash-table :test 'equal) :type hash-table)

  ;; Tip management
  (tips nil :type list)
  (tips-cache-valid nil :type boolean)

  ;; Finality tracking
  (finalized (make-hash-table :test 'equalp) :type hash-table)
  (finality-window 100 :type (unsigned-byte 64))

  ;; GHOSTDAG parameters
  (k-param +default-k-param+ :type (unsigned-byte 32))

  ;; Threading (portable - uses a simple lock)
  (lock nil :type t)

  ;; Statistics
  (vertex-count 0 :type (unsigned-byte 64))
  (edge-count 0 :type (unsigned-byte 64))
  (max-height 0 :type (unsigned-byte 64))
  (total-work 0 :type (unsigned-byte 256)))

(defun make-dag-consensus (&key
                            (k-param +default-k-param+)
                            (finality-window 100))
  "Create a new DAG consensus structure."
  (%make-dag-consensus
   :k-param k-param
   :finality-window finality-window
   :lock (make-dag-lock)))

(defun print-dag-consensus (dag stream depth)
  "Print DAG consensus summary."
  (declare (ignore depth))
  (format stream "#<DAG-CONSENSUS vertices=~D edges=~D tips=~D height=~D>"
          (dag-consensus-vertex-count dag)
          (dag-consensus-edge-count dag)
          (length (dag-consensus-tips dag))
          (dag-consensus-max-height dag)))

;;; ============================================================================
;;; Portable Lock Implementation
;;; ============================================================================

(defun make-dag-lock ()
  "Create a lock for thread safety. Returns NIL for single-threaded use."
  #+sbcl (sb-thread:make-recursive-lock "dag-consensus-lock")
  #-sbcl nil)

(defmacro with-dag-lock ((dag) &body body)
  "Execute body with DAG lock held."
  (let ((lock-var (gensym "LOCK")))
    `(let ((,lock-var (dag-consensus-lock ,dag)))
       #+sbcl
       (if ,lock-var
           (sb-thread:with-recursive-lock (,lock-var)
             ,@body)
           (progn ,@body))
       #-sbcl
       (progn ,@body))))

;;; ============================================================================
;;; Vertex Operations
;;; ============================================================================

(defun get-vertex (dag vertex-id)
  "Get a vertex from the DAG by its ID."
  (gethash vertex-id (dag-consensus-vertices dag)))

(defun get-vertices (dag)
  "Get all vertices in the DAG as a list."
  (let ((vertices nil))
    (maphash (lambda (id vertex)
               (declare (ignore id))
               (push vertex vertices))
             (dag-consensus-vertices dag))
    vertices))

(defun add-vertex (dag vertex)
  "Add a vertex to the DAG with parent connections."
  (with-dag-lock (dag)
    (let ((vertex-id (dag-vertex-id vertex)))
      ;; Check for duplicate
      (when (gethash vertex-id (dag-consensus-vertices dag))
        (return-from add-vertex nil))

      ;; Check for cycles before adding
      (when (would-create-cycle-p dag vertex)
        (error "Adding vertex ~A would create a cycle"
               (hash-to-hex vertex-id)))

      ;; Add vertex to table
      (setf (gethash vertex-id (dag-consensus-vertices dag)) vertex)

      ;; Connect to parents
      (dolist (parent-id (dag-vertex-parents vertex))
        (let ((parent (gethash parent-id (dag-consensus-vertices dag))))
          (when parent
            ;; Add child reference to parent
            (push vertex-id (dag-vertex-children parent))
            ;; Create edge
            (let ((edge (make-dag-edge
                         :from vertex-id
                         :to parent-id
                         :weight (dag-vertex-work vertex)
                         :type (if (dag-vertex-is-merge-p vertex)
                                   :merge
                                   :parent))))
              (setf (gethash (cons vertex-id parent-id)
                             (dag-consensus-edges dag))
                    edge)
              (incf (dag-consensus-edge-count dag))))))

      ;; Update statistics
      (incf (dag-consensus-vertex-count dag))
      (when (> (dag-vertex-height vertex) (dag-consensus-max-height dag))
        (setf (dag-consensus-max-height dag) (dag-vertex-height vertex)))
      (incf (dag-consensus-total-work dag) (dag-vertex-work vertex))

      ;; Invalidate tips cache
      (setf (dag-consensus-tips-cache-valid dag) nil)

      ;; Call event hook
      (when *on-vertex-added*
        (funcall *on-vertex-added* vertex dag))

      t)))

(defun add-edge (dag from-id to-id &key (weight 1) (type :parent))
  "Add an edge between two vertices."
  (with-dag-lock (dag)
    (let ((edge-key (cons from-id to-id)))
      (unless (gethash edge-key (dag-consensus-edges dag))
        (let ((edge (make-dag-edge
                     :from from-id
                     :to to-id
                     :weight weight
                     :type type)))
          (setf (gethash edge-key (dag-consensus-edges dag)) edge)
          (incf (dag-consensus-edge-count dag))
          t)))))

(defun remove-vertex (dag vertex-id)
  "Remove a vertex from the DAG (for pruning)."
  (with-dag-lock (dag)
    (let ((vertex (gethash vertex-id (dag-consensus-vertices dag))))
      (unless vertex
        (return-from remove-vertex nil))

      ;; Check for children
      (when (dag-vertex-children vertex)
        (error "Cannot remove vertex ~A with children"
               (hash-to-hex vertex-id)))

      ;; Remove from parents' children lists
      (dolist (parent-id (dag-vertex-parents vertex))
        (let ((parent (gethash parent-id (dag-consensus-vertices dag))))
          (when parent
            (setf (dag-vertex-children parent)
                  (remove vertex-id (dag-vertex-children parent)
                          :test #'hash-equal-p)))))

      ;; Remove edges
      (dolist (parent-id (dag-vertex-parents vertex))
        (let ((edge-key (cons vertex-id parent-id)))
          (when (gethash edge-key (dag-consensus-edges dag))
            (remhash edge-key (dag-consensus-edges dag))
            (decf (dag-consensus-edge-count dag)))))

      ;; Remove vertex
      (remhash vertex-id (dag-consensus-vertices dag))
      (decf (dag-consensus-vertex-count dag))
      (decf (dag-consensus-total-work dag) (dag-vertex-work vertex))

      ;; Invalidate tips cache
      (setf (dag-consensus-tips-cache-valid dag) nil)

      t)))

(defun connect-parents (dag vertex parent-ids)
  "Connect a vertex to additional parents."
  (with-dag-lock (dag)
    (let ((vertex-id (dag-vertex-id vertex))
          (connected 0))
      (dolist (parent-id parent-ids)
        (unless (member parent-id (dag-vertex-parents vertex)
                        :test #'hash-equal-p)
          ;; Check for cycle
          (when (is-ancestor-p dag vertex-id parent-id)
            (error "Connection would create cycle: ~A -> ~A"
                   (hash-to-hex vertex-id)
                   (hash-to-hex parent-id)))

          ;; Add connection
          (let ((parent (gethash parent-id (dag-consensus-vertices dag))))
            (when parent
              (push parent-id (dag-vertex-parents vertex))
              (push vertex-id (dag-vertex-children parent))
              (let ((edge (make-dag-edge
                           :from vertex-id
                           :to parent-id
                           :type :parent)))
                (setf (gethash (cons vertex-id parent-id)
                               (dag-consensus-edges dag))
                      edge)
                (incf (dag-consensus-edge-count dag))
                (incf connected))))))
      connected)))

;;; ============================================================================
;;; Graph Traversal
;;; ============================================================================

(defun get-parents (dag vertex-id)
  "Get all parent vertices of a vertex."
  (let ((vertex (get-vertex dag vertex-id)))
    (when vertex
      (mapcar (lambda (parent-id)
                (gethash parent-id (dag-consensus-vertices dag)))
              (dag-vertex-parents vertex)))))

(defun get-children (dag vertex-id)
  "Get all child vertices of a vertex."
  (let ((vertex (get-vertex dag vertex-id)))
    (when vertex
      (mapcar (lambda (child-id)
                (gethash child-id (dag-consensus-vertices dag)))
              (dag-vertex-children vertex)))))

(defun get-ancestors (dag vertex-id &key (max-depth nil))
  "Get all ancestors of a vertex (transitive parents)."
  (let ((visited (make-hash-table :test 'equalp))
        (ancestors nil)
        (queue (list (cons vertex-id 0))))
    (loop while queue do
      (destructuring-bind (current-id . depth) (pop queue)
        (unless (gethash current-id visited)
          (setf (gethash current-id visited) t)
          (let ((vertex (get-vertex dag current-id)))
            (when vertex
              ;; Skip the starting vertex
              (unless (hash-equal-p current-id vertex-id)
                (push vertex ancestors))
              ;; Add parents to queue if within depth limit
              (when (or (null max-depth) (< depth max-depth))
                (dolist (parent-id (dag-vertex-parents vertex))
                  (unless (gethash parent-id visited)
                    (push (cons parent-id (1+ depth)) queue)))))))))
    (nreverse ancestors)))

(defun get-descendants (dag vertex-id &key (max-depth nil))
  "Get all descendants of a vertex (transitive children)."
  (let ((visited (make-hash-table :test 'equalp))
        (descendants nil)
        (queue (list (cons vertex-id 0))))
    (loop while queue do
      (destructuring-bind (current-id . depth) (pop queue)
        (unless (gethash current-id visited)
          (setf (gethash current-id visited) t)
          (let ((vertex (get-vertex dag current-id)))
            (when vertex
              ;; Skip the starting vertex
              (unless (hash-equal-p current-id vertex-id)
                (push vertex descendants))
              ;; Add children to queue if within depth limit
              (when (or (null max-depth) (< depth max-depth))
                (dolist (child-id (dag-vertex-children vertex))
                  (unless (gethash child-id visited)
                    (push (cons child-id (1+ depth)) queue)))))))))
    (nreverse descendants)))

;;; ============================================================================
;;; Tip Management
;;; ============================================================================

(defun get-tips (dag)
  "Get all current tips (vertices with no children)."
  (with-dag-lock (dag)
    (unless (dag-consensus-tips-cache-valid dag)
      ;; Recompute tips
      (let ((tips nil))
        (maphash (lambda (id vertex)
                   (declare (ignore id))
                   (when (null (dag-vertex-children vertex))
                     (push (dag-vertex-id vertex) tips)))
                 (dag-consensus-vertices dag))
        (setf (dag-consensus-tips dag) tips
              (dag-consensus-tips-cache-valid dag) t)))
    ;; Return vertices for tip IDs
    (mapcar (lambda (tip-id)
              (gethash tip-id (dag-consensus-vertices dag)))
            (dag-consensus-tips dag))))

(defun is-tip-p (dag vertex-id)
  "Check if a vertex is a current tip."
  (let ((vertex (get-vertex dag vertex-id)))
    (and vertex (null (dag-vertex-children vertex)))))

(defun get-genesis (dag)
  "Get the genesis vertex of the DAG."
  (dag-consensus-genesis dag))

;;; ============================================================================
;;; Graph Properties
;;; ============================================================================

(defun is-ancestor-p (dag vertex-id potential-ancestor-id)
  "Check if potential-ancestor-id is an ancestor of vertex-id."
  (let ((visited (make-hash-table :test 'equalp))
        (queue (list vertex-id)))
    (loop while queue do
      (let ((current-id (pop queue)))
        (cond
          ((hash-equal-p current-id potential-ancestor-id)
           (return-from is-ancestor-p t))
          ((gethash current-id visited)
           nil)
          (t
           (setf (gethash current-id visited) t)
           (let ((vertex (get-vertex dag current-id)))
             (when vertex
               (dolist (parent-id (dag-vertex-parents vertex))
                 (push parent-id queue))))))))
    nil))

(defun is-descendant-p (dag vertex-id potential-descendant-id)
  "Check if potential-descendant-id is a descendant of vertex-id."
  (is-ancestor-p dag potential-descendant-id vertex-id))

(defun reachable-p (dag from-id to-id)
  "Check if to-id is reachable from from-id (ancestor or descendant)."
  (or (is-ancestor-p dag from-id to-id)
      (is-descendant-p dag from-id to-id)))

(defun is-orphan-p (dag vertex-id)
  "Check if vertex has missing parent references."
  (let ((vertex (get-vertex dag vertex-id)))
    (when vertex
      (some (lambda (parent-id)
              (null (get-vertex dag parent-id)))
            (dag-vertex-parents vertex)))))

(defun would-create-cycle-p (dag new-vertex)
  "Check if adding new-vertex would create a cycle."
  (let ((new-id (dag-vertex-id new-vertex)))
    (dolist (parent-id (dag-vertex-parents new-vertex))
      (when (is-ancestor-p dag parent-id new-id)
        (return-from would-create-cycle-p t)))
    nil))

(defun has-cycle-p (dag)
  "Check if the DAG contains any cycles."
  (let ((white (make-hash-table :test 'equalp))
        (gray (make-hash-table :test 'equalp))
        (black (make-hash-table :test 'equalp)))
    ;; Initialize all vertices as white
    (maphash (lambda (id vertex)
               (declare (ignore vertex))
               (setf (gethash id white) t))
             (dag-consensus-vertices dag))

    ;; DFS from each unvisited vertex
    (labels ((dfs (vertex-id)
               (when (gethash vertex-id gray)
                 (return-from has-cycle-p t))
               (when (gethash vertex-id black)
                 (return-from dfs nil))

               (remhash vertex-id white)
               (setf (gethash vertex-id gray) t)

               (let ((vertex (get-vertex dag vertex-id)))
                 (when vertex
                   (dolist (parent-id (dag-vertex-parents vertex))
                     (dfs parent-id))))

               (remhash vertex-id gray)
               (setf (gethash vertex-id black) t)))
      (maphash (lambda (id value)
                 (declare (ignore value))
                 (dfs id))
               white))
    nil))

(defun is-valid-dag-p (dag)
  "Validate the DAG structure (no cycles, connected, valid references)."
  (and (not (has-cycle-p dag))
       (dag-consensus-genesis dag)
       (plusp (dag-consensus-vertex-count dag))))

;;; ============================================================================
;;; Statistics
;;; ============================================================================

(defun vertex-count (dag)
  "Get the number of vertices in the DAG."
  (dag-consensus-vertex-count dag))

(defun edge-count (dag)
  "Get the number of edges in the DAG."
  (dag-consensus-edge-count dag))

(defun dag-statistics (dag)
  "Compute comprehensive DAG statistics."
  (with-dag-lock (dag)
    (let ((tips (get-tips dag))
          (total-parents 0)
          (max-parents 0)
          (merge-count 0))
      ;; Compute parent statistics
      (maphash (lambda (id vertex)
                 (declare (ignore id))
                 (let ((parent-count (length (dag-vertex-parents vertex))))
                   (incf total-parents parent-count)
                   (when (> parent-count max-parents)
                     (setf max-parents parent-count))
                   (when (dag-vertex-is-merge-p vertex)
                     (incf merge-count))))
               (dag-consensus-vertices dag))

      (list :vertex-count (dag-consensus-vertex-count dag)
            :edge-count (dag-consensus-edge-count dag)
            :tip-count (length tips)
            :max-height (dag-consensus-max-height dag)
            :total-work (dag-consensus-total-work dag)
            :avg-parents (if (plusp (dag-consensus-vertex-count dag))
                             (/ (float total-parents)
                                (dag-consensus-vertex-count dag))
                             0.0)
            :max-parents max-parents
            :merge-blocks merge-count
            :finalized-count (hash-table-count (dag-consensus-finalized dag))
            :k-param (dag-consensus-k-param dag)))))

(defun avg-parents-per-block (dag)
  "Calculate average number of parents per block."
  (getf (dag-statistics dag) :avg-parents))

(defun dag-width (dag)
  "Calculate DAG width (number of tips)."
  (length (get-tips dag)))

(defun dag-depth (dag)
  "Calculate DAG depth (maximum height)."
  (dag-consensus-max-height dag))

;;; ============================================================================
;;; Serialization
;;; ============================================================================

(defun serialize-vertex (vertex)
  "Serialize a DAG vertex to an alist."
  (list :id (hash-to-hex (dag-vertex-id vertex))
        :hash (hash-to-hex (dag-vertex-hash vertex))
        :height (dag-vertex-height vertex)
        :timestamp (dag-vertex-timestamp vertex)
        :work (dag-vertex-work vertex)
        :parents (mapcar #'hash-to-hex (dag-vertex-parents vertex))
        :color (dag-vertex-color vertex)
        :score (dag-vertex-score vertex)
        :is-merge (dag-vertex-is-merge-p vertex)))

(defun deserialize-vertex (alist)
  "Deserialize a DAG vertex from an alist."
  (make-dag-vertex
   :id (hex-to-hash (getf alist :id))
   :hash (hex-to-hash (getf alist :hash))
   :height (getf alist :height)
   :timestamp (getf alist :timestamp)
   :work (getf alist :work)
   :parents (mapcar #'hex-to-hash (getf alist :parents))
   :is-merge-p (getf alist :is-merge)))

(defun serialize-dag (dag)
  "Serialize entire DAG to alist format."
  (let ((vertices nil))
    (maphash (lambda (id vertex)
               (declare (ignore id))
               (push (serialize-vertex vertex) vertices))
             (dag-consensus-vertices dag))
    (list :genesis (when (dag-consensus-genesis dag)
                     (hash-to-hex (dag-vertex-id (dag-consensus-genesis dag))))
          :k-param (dag-consensus-k-param dag)
          :vertices vertices
          :stats (dag-statistics dag))))

(defun deserialize-dag (alist)
  "Deserialize a DAG from alist format."
  (let ((dag (make-dag-consensus
              :k-param (getf alist :k-param +default-k-param+))))
    ;; Add vertices
    (dolist (vertex-data (getf alist :vertices))
      (let ((vertex (deserialize-vertex vertex-data)))
        (add-vertex dag vertex)
        ;; Set genesis if this is it
        (when (and (getf alist :genesis)
                   (string= (getf vertex-data :id) (getf alist :genesis)))
          (setf (dag-consensus-genesis dag) vertex))))
    dag))

;;; ============================================================================
;;; Validation
;;; ============================================================================

(defun validate-dag-structure (dag)
  "Validate the entire DAG structure for consistency."
  (let ((errors nil)
        (warnings nil))

    ;; Check for cycles
    (when (has-cycle-p dag)
      (push "DAG contains a cycle" errors))

    ;; Check genesis
    (unless (dag-consensus-genesis dag)
      (push "DAG has no genesis vertex" errors))

    ;; Check all parent references
    (maphash (lambda (id vertex)
               (declare (ignore id))
               (dolist (parent-id (dag-vertex-parents vertex))
                 (unless (get-vertex dag parent-id)
                   (push (format nil "Missing parent ~A for vertex ~A"
                                 (hash-to-hex parent-id)
                                 (hash-to-hex (dag-vertex-id vertex)))
                         warnings))))
             (dag-consensus-vertices dag))

    ;; Check height consistency
    (maphash (lambda (id vertex)
               (declare (ignore id))
               (let ((max-parent-height 0))
                 (dolist (parent-id (dag-vertex-parents vertex))
                   (let ((parent (get-vertex dag parent-id)))
                     (when (and parent
                                (> (dag-vertex-height parent) max-parent-height))
                       (setf max-parent-height (dag-vertex-height parent)))))
                 (unless (or (null (dag-vertex-parents vertex))
                             (= (dag-vertex-height vertex) (1+ max-parent-height)))
                   (push (format nil "Height inconsistency for vertex ~A"
                                 (hash-to-hex (dag-vertex-id vertex)))
                         warnings))))
             (dag-consensus-vertices dag))

    (make-dag-validation-result
     :valid-p (null errors)
     :errors (nreverse errors)
     :warnings (nreverse warnings)
     :checked-properties '(:acyclicity :genesis :parent-refs :height-consistency))))

(defun verify-acyclicity (dag)
  "Verify that the DAG has no cycles."
  (not (has-cycle-p dag)))

;;; ============================================================================
;;; Display Functions
;;; ============================================================================

(defun print-dag-summary (dag &optional (stream *standard-output*))
  "Print a summary of the DAG to stream."
  (let ((stats (dag-statistics dag)))
    (format stream "~&=== DAG Consensus Summary ===~%")
    (format stream "Vertices:     ~D~%" (getf stats :vertex-count))
    (format stream "Edges:        ~D~%" (getf stats :edge-count))
    (format stream "Tips:         ~D~%" (getf stats :tip-count))
    (format stream "Max Height:   ~D~%" (getf stats :max-height))
    (format stream "Avg Parents:  ~,2F~%" (getf stats :avg-parents))
    (format stream "Merge Blocks: ~D~%" (getf stats :merge-blocks))
    (format stream "Finalized:    ~D~%" (getf stats :finalized-count))
    (format stream "K Parameter:  ~D~%" (getf stats :k-param))))

(defun print-vertex-info (dag vertex-id &optional (stream *standard-output*))
  "Print detailed information about a vertex."
  (let ((vertex (get-vertex dag vertex-id)))
    (if vertex
        (progn
          (format stream "~&=== Vertex Info ===~%")
          (format stream "ID:       ~A~%" (hash-to-hex (dag-vertex-id vertex)))
          (format stream "Hash:     ~A~%" (hash-to-hex (dag-vertex-hash vertex)))
          (format stream "Height:   ~D~%" (dag-vertex-height vertex))
          (format stream "Time:     ~D~%" (dag-vertex-timestamp vertex))
          (format stream "Work:     ~D~%" (dag-vertex-work vertex))
          (format stream "Parents:  ~D~%" (length (dag-vertex-parents vertex)))
          (format stream "Children: ~D~%" (length (dag-vertex-children vertex)))
          (format stream "Color:    ~A~%" (dag-vertex-color vertex))
          (format stream "Score:    ~D~%" (dag-vertex-score vertex))
          (format stream "Is Merge: ~A~%" (dag-vertex-is-merge-p vertex)))
        (format stream "Vertex not found: ~A~%" (hash-to-hex vertex-id)))))

(defun print-tip-selection (dag &optional (stream *standard-output*))
  "Print current tips and their weights."
  (format stream "~&=== Current Tips ===~%")
  (dolist (tip (get-tips dag))
    (format stream "~A h=~D score=~D work=~D~%"
            (subseq (hash-to-hex (dag-vertex-id tip)) 0 16)
            (dag-vertex-height tip)
            (dag-vertex-score tip)
            (dag-vertex-cumulative-work tip))))
