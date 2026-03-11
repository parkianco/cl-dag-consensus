;;;; test-dag.lisp - Tests for cl-dag-consensus
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-dag-consensus.test)

;;; ============================================================================
;;; Test Framework
;;; ============================================================================

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defmacro deftest (name &body body)
  "Define a test case."
  `(defun ,name ()
     (incf *test-count*)
     (handler-case
         (progn
           ,@body
           (incf *pass-count*)
           (format t "~&  PASS: ~A~%" ',name)
           t)
       (error (e)
         (incf *fail-count*)
         (format t "~&  FAIL: ~A - ~A~%" ',name e)
         nil))))

(defmacro assert-true (form &optional message)
  "Assert that form evaluates to true."
  `(unless ,form
     (error "~A: expected true, got ~A" (or ,message ',form) ,form)))

(defmacro assert-false (form &optional message)
  "Assert that form evaluates to false."
  `(when ,form
     (error "~A: expected false, got ~A" (or ,message ',form) ,form)))

(defmacro assert-equal (expected actual &optional message)
  "Assert that expected equals actual."
  `(unless (equal ,expected ,actual)
     (error "~A: expected ~A, got ~A" (or ,message 'assert-equal) ,expected ,actual)))

(defmacro assert-eql (expected actual &optional message)
  "Assert that expected eql actual."
  `(unless (eql ,expected ,actual)
     (error "~A: expected ~A, got ~A" (or ,message 'assert-eql) ,expected ,actual)))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun make-test-hash (byte-value)
  "Create a test hash filled with a single byte value."
  (let ((hash (make-array 32 :element-type '(unsigned-byte 8)
                              :initial-element byte-value)))
    hash))

(defun make-genesis-vertex ()
  "Create a genesis vertex for testing."
  (let ((genesis-hash (make-test-hash 0)))
    (make-dag-vertex
     :id genesis-hash
     :hash genesis-hash
     :height 0
     :timestamp 1000
     :work 1000
     :parents nil
     :is-merge-p nil)))

(defun make-test-dag-with-genesis ()
  "Create a DAG with a genesis vertex."
  (let ((dag (make-dag-consensus))
        (genesis (make-genesis-vertex)))
    (add-vertex dag genesis)
    (setf (dag-consensus-genesis dag) genesis)
    dag))

;;; ============================================================================
;;; Utility Tests
;;; ============================================================================

(deftest test-zero-hash
  (let ((hash (zero-hash)))
    (assert-eql 32 (length hash))
    (assert-true (every #'zerop hash))))

(deftest test-hash-equal-p
  (let ((h1 (make-test-hash 1))
        (h2 (make-test-hash 1))
        (h3 (make-test-hash 2)))
    (assert-true (hash-equal-p h1 h2))
    (assert-false (hash-equal-p h1 h3))))

(deftest test-copy-hash
  (let* ((h1 (make-test-hash 42))
         (h2 (copy-hash h1)))
    (assert-true (hash-equal-p h1 h2))
    (setf (aref h2 0) 99)
    (assert-false (hash-equal-p h1 h2))))

(deftest test-hash-hex-roundtrip
  (let* ((h1 (make-test-hash 171))  ; 0xAB
         (hex (hash-to-hex h1))
         (h2 (hex-to-hash hex)))
    (assert-true (hash-equal-p h1 h2))))

(deftest test-hash-less-p
  (let ((h1 (make-test-hash 1))
        (h2 (make-test-hash 2)))
    (assert-true (hash-less-p h1 h2))
    (assert-false (hash-less-p h2 h1))
    (assert-false (hash-less-p h1 h1))))

;;; ============================================================================
;;; Vertex Tests
;;; ============================================================================

(deftest test-make-dag-vertex
  (let ((vertex (make-dag-vertex
                 :id (make-test-hash 1)
                 :hash (make-test-hash 1)
                 :height 5
                 :timestamp 12345
                 :work 100)))
    (assert-true (dag-vertex-p vertex))
    (assert-eql 5 (dag-vertex-height vertex))
    (assert-eql 12345 (dag-vertex-timestamp vertex))
    (assert-eql 100 (dag-vertex-work vertex))))

(deftest test-dag-vertex-parents
  (let* ((parent-hash (make-test-hash 1))
         (vertex (make-dag-vertex
                  :id (make-test-hash 2)
                  :hash (make-test-hash 2)
                  :height 1
                  :parents (list parent-hash))))
    (assert-eql 1 (length (dag-vertex-parents vertex)))
    (assert-true (hash-equal-p parent-hash (first (dag-vertex-parents vertex))))))

(deftest test-make-dag-edge
  (let ((edge (make-dag-edge
               :from (make-test-hash 2)
               :to (make-test-hash 1)
               :weight 50
               :type :parent)))
    (assert-true (dag-edge-p edge))
    (assert-eql 50 (dag-edge-weight edge))
    (assert-eql :parent (dag-edge-type edge))))

;;; ============================================================================
;;; Block Tests
;;; ============================================================================

(deftest test-make-dag-block
  (let ((block (make-dag-block
                :version 1
                :parent-hashes (list (make-test-hash 1))
                :timestamp 12345
                :nonce 42)))
    (assert-true (dag-block-p block))
    (assert-eql 1 (dag-block-version block))
    (assert-eql 12345 (dag-block-timestamp block))
    (assert-eql 42 (dag-block-nonce block))))

(deftest test-is-genesis-block-p
  (let ((genesis (make-dag-block :version 1 :parent-hashes nil))
        (normal (make-dag-block :version 1 :parent-hashes (list (make-test-hash 1)))))
    (assert-true (is-genesis-block-p genesis))
    (assert-false (is-genesis-block-p normal))))

(deftest test-parent-count
  (let ((block1 (make-dag-block :parent-hashes nil))
        (block2 (make-dag-block :parent-hashes (list (make-test-hash 1))))
        (block3 (make-dag-block :parent-hashes (list (make-test-hash 1)
                                                      (make-test-hash 2)))))
    (assert-eql 0 (parent-count block1))
    (assert-eql 1 (parent-count block2))
    (assert-eql 2 (parent-count block3))))

(deftest test-is-multi-parent-p
  (let ((single (make-dag-block :parent-hashes (list (make-test-hash 1))))
        (multi (make-dag-block :parent-hashes (list (make-test-hash 1)
                                                     (make-test-hash 2)))))
    (assert-false (is-multi-parent-p single))
    (assert-true (is-multi-parent-p multi))))

;;; ============================================================================
;;; DAG Structure Tests
;;; ============================================================================

(deftest test-make-dag-consensus
  (let ((dag (make-dag-consensus :k-param 20 :finality-window 50)))
    (assert-true (dag-consensus-p dag))
    (assert-eql 20 (dag-consensus-k-param dag))
    (assert-eql 50 (dag-consensus-finality-window dag))))

(deftest test-add-vertex
  (let ((dag (make-dag-consensus))
        (vertex (make-genesis-vertex)))
    (assert-true (add-vertex dag vertex))
    (assert-eql 1 (vertex-count dag))
    ;; Cannot add duplicate
    (assert-false (add-vertex dag vertex))))

(deftest test-get-vertex
  (let* ((dag (make-test-dag-with-genesis))
         (genesis (dag-consensus-genesis dag))
         (found (get-vertex dag (dag-vertex-id genesis))))
    (assert-true found)
    (assert-true (hash-equal-p (dag-vertex-id genesis) (dag-vertex-id found)))))

(deftest test-get-vertices
  (let ((dag (make-test-dag-with-genesis)))
    (let ((vertices (get-vertices dag)))
      (assert-eql 1 (length vertices)))))

(deftest test-vertex-count-edge-count
  (let* ((dag (make-test-dag-with-genesis))
         (genesis (dag-consensus-genesis dag))
         (child (make-dag-vertex
                 :id (make-test-hash 1)
                 :hash (make-test-hash 1)
                 :height 1
                 :work 100
                 :parents (list (dag-vertex-id genesis)))))
    (add-vertex dag child)
    (assert-eql 2 (vertex-count dag))
    (assert-eql 1 (edge-count dag))))

;;; ============================================================================
;;; Graph Traversal Tests
;;; ============================================================================

(deftest test-get-tips
  (let* ((dag (make-test-dag-with-genesis))
         (tips (get-tips dag)))
    (assert-eql 1 (length tips))))

(deftest test-is-tip-p
  (let* ((dag (make-test-dag-with-genesis))
         (genesis (dag-consensus-genesis dag)))
    (assert-true (is-tip-p dag (dag-vertex-id genesis)))))

(deftest test-get-parents-children
  (let* ((dag (make-test-dag-with-genesis))
         (genesis (dag-consensus-genesis dag))
         (child (make-dag-vertex
                 :id (make-test-hash 1)
                 :hash (make-test-hash 1)
                 :height 1
                 :work 100
                 :parents (list (dag-vertex-id genesis)))))
    (add-vertex dag child)
    (let ((parents (get-parents dag (dag-vertex-id child)))
          (children (get-children dag (dag-vertex-id genesis))))
      (assert-eql 1 (length parents))
      (assert-eql 1 (length children)))))

(deftest test-is-ancestor-p
  (let* ((dag (make-test-dag-with-genesis))
         (genesis (dag-consensus-genesis dag))
         (child (make-dag-vertex
                 :id (make-test-hash 1)
                 :hash (make-test-hash 1)
                 :height 1
                 :work 100
                 :parents (list (dag-vertex-id genesis)))))
    (add-vertex dag child)
    (assert-true (is-ancestor-p dag (dag-vertex-id child) (dag-vertex-id genesis)))
    (assert-false (is-ancestor-p dag (dag-vertex-id genesis) (dag-vertex-id child)))))

(deftest test-is-valid-dag-p
  (let ((dag (make-test-dag-with-genesis)))
    (assert-true (is-valid-dag-p dag))))

(deftest test-has-cycle-p
  (let ((dag (make-test-dag-with-genesis)))
    (assert-false (has-cycle-p dag))))

;;; ============================================================================
;;; Ordering Tests
;;; ============================================================================

(deftest test-topological-sort
  (let* ((dag (make-test-dag-with-genesis))
         (genesis (dag-consensus-genesis dag))
         (child (make-dag-vertex
                 :id (make-test-hash 1)
                 :hash (make-test-hash 1)
                 :height 1
                 :work 100
                 :parents (list (dag-vertex-id genesis)))))
    (add-vertex dag child)
    (let ((sorted (topological-sort dag)))
      (assert-eql 2 (length sorted))
      ;; Genesis should come before child in topological order
      (assert-eql 0 (dag-vertex-height (first sorted))))))

(deftest test-calculate-branch-weight
  (let* ((dag (make-test-dag-with-genesis))
         (genesis (dag-consensus-genesis dag))
         (child (make-dag-vertex
                 :id (make-test-hash 1)
                 :hash (make-test-hash 1)
                 :height 1
                 :work 500
                 :parents (list (dag-vertex-id genesis)))))
    (add-vertex dag child)
    (let ((weight (calculate-branch-weight dag (dag-vertex-id child))))
      ;; Weight should be genesis work (1000) + child work (500) = 1500
      (assert-eql 1500 weight))))

(deftest test-select-best-tip
  (let* ((dag (make-test-dag-with-genesis))
         (genesis (dag-consensus-genesis dag))
         (child1 (make-dag-vertex
                  :id (make-test-hash 1)
                  :hash (make-test-hash 1)
                  :height 1
                  :work 100
                  :parents (list (dag-vertex-id genesis))))
         (child2 (make-dag-vertex
                  :id (make-test-hash 2)
                  :hash (make-test-hash 2)
                  :height 1
                  :work 200
                  :parents (list (dag-vertex-id genesis)))))
    (add-vertex dag child1)
    (add-vertex dag child2)
    (update-weights dag)
    (let ((best (select-best-tip dag)))
      ;; child2 has more work
      (assert-true (hash-equal-p (dag-vertex-id child2) (dag-vertex-id best))))))

;;; ============================================================================
;;; GHOSTDAG Tests
;;; ============================================================================

(deftest test-ghostdag-ordering
  (let* ((dag (make-test-dag-with-genesis))
         (genesis (dag-consensus-genesis dag))
         (child (make-dag-vertex
                 :id (make-test-hash 1)
                 :hash (make-test-hash 1)
                 :height 1
                 :work 100
                 :parents (list (dag-vertex-id genesis)))))
    (add-vertex dag child)
    (let ((ordered (ghostdag-ordering dag)))
      (assert-eql 2 (length ordered))
      ;; Genesis should be colored blue
      (assert-eql :blue (dag-vertex-color genesis)))))

(deftest test-calculate-blue-score
  (let* ((dag (make-test-dag-with-genesis))
         (genesis (dag-consensus-genesis dag)))
    ;; Genesis blue score should be 0
    (calculate-blue-score dag (dag-vertex-id genesis))
    (assert-eql 0 (dag-vertex-score genesis))))

;;; ============================================================================
;;; Finality Tests
;;; ============================================================================

(deftest test-finality-depth
  (let* ((dag (make-test-dag-with-genesis))
         (genesis (dag-consensus-genesis dag)))
    ;; Genesis has no descendants initially
    (assert-eql 0 (finality-depth dag (dag-vertex-id genesis)))
    ;; Add a child
    (let ((child (make-dag-vertex
                  :id (make-test-hash 1)
                  :hash (make-test-hash 1)
                  :height 1
                  :work 100
                  :parents (list (dag-vertex-id genesis)))))
      (add-vertex dag child)
      (assert-eql 1 (finality-depth dag (dag-vertex-id genesis))))))

;;; ============================================================================
;;; Serialization Tests
;;; ============================================================================

(deftest test-serialize-deserialize-vertex
  (let* ((vertex (make-dag-vertex
                  :id (make-test-hash 1)
                  :hash (make-test-hash 1)
                  :height 5
                  :timestamp 12345
                  :work 100))
         (serialized (serialize-vertex vertex))
         (deserialized (deserialize-vertex serialized)))
    (assert-eql (dag-vertex-height vertex) (dag-vertex-height deserialized))
    (assert-eql (dag-vertex-timestamp vertex) (dag-vertex-timestamp deserialized))
    (assert-eql (dag-vertex-work vertex) (dag-vertex-work deserialized))))

(deftest test-serialize-deserialize-dag
  (let* ((dag (make-test-dag-with-genesis))
         (serialized (serialize-dag dag))
         (deserialized (deserialize-dag serialized)))
    (assert-eql (vertex-count dag) (vertex-count deserialized))))

;;; ============================================================================
;;; Validation Tests
;;; ============================================================================

(deftest test-validate-dag-structure
  (let* ((dag (make-test-dag-with-genesis))
         (result (validate-dag-structure dag)))
    (assert-true (dag-validation-result-valid-p result))
    (assert-true (null (dag-validation-result-errors result)))))

(deftest test-verify-acyclicity
  (let ((dag (make-test-dag-with-genesis)))
    (assert-true (verify-acyclicity dag))))

;;; ============================================================================
;;; Statistics Tests
;;; ============================================================================

(deftest test-dag-statistics
  (let* ((dag (make-test-dag-with-genesis))
         (stats (dag-statistics dag)))
    (assert-eql 1 (getf stats :vertex-count))
    (assert-eql 0 (getf stats :edge-count))
    (assert-eql 1 (getf stats :tip-count))))

(deftest test-dag-width-depth
  (let ((dag (make-test-dag-with-genesis)))
    (assert-eql 1 (dag-width dag))
    (assert-eql 0 (dag-depth dag))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-tests ()
  "Run all tests and report results."
  (setf *test-count* 0
        *pass-count* 0
        *fail-count* 0)

  (format t "~&Running cl-dag-consensus tests...~%")
  (format t "~&=================================~%")

  ;; Utility tests
  (test-zero-hash)
  (test-hash-equal-p)
  (test-copy-hash)
  (test-hash-hex-roundtrip)
  (test-hash-less-p)

  ;; Vertex tests
  (test-make-dag-vertex)
  (test-dag-vertex-parents)
  (test-make-dag-edge)

  ;; Block tests
  (test-make-dag-block)
  (test-is-genesis-block-p)
  (test-parent-count)
  (test-is-multi-parent-p)

  ;; DAG structure tests
  (test-make-dag-consensus)
  (test-add-vertex)
  (test-get-vertex)
  (test-get-vertices)
  (test-vertex-count-edge-count)

  ;; Graph traversal tests
  (test-get-tips)
  (test-is-tip-p)
  (test-get-parents-children)
  (test-is-ancestor-p)
  (test-is-valid-dag-p)
  (test-has-cycle-p)

  ;; Ordering tests
  (test-topological-sort)
  (test-calculate-branch-weight)
  (test-select-best-tip)

  ;; GHOSTDAG tests
  (test-ghostdag-ordering)
  (test-calculate-blue-score)

  ;; Finality tests
  (test-finality-depth)

  ;; Serialization tests
  (test-serialize-deserialize-vertex)
  (test-serialize-deserialize-dag)

  ;; Validation tests
  (test-validate-dag-structure)
  (test-verify-acyclicity)

  ;; Statistics tests
  (test-dag-statistics)
  (test-dag-width-depth)

  ;; Report
  (format t "~&=================================~%")
  (format t "~&Tests: ~D  Passed: ~D  Failed: ~D~%"
          *test-count* *pass-count* *fail-count*)
  (if (zerop *fail-count*)
      (format t "~&All tests passed!~%")
      (format t "~&~D test(s) FAILED~%" *fail-count*))

  (zerop *fail-count*))
