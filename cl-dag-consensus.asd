;;;; cl-dag-consensus.asd - DAG-based consensus protocol for Common Lisp
;;;; SPDX-License-Identifier: MIT

(defsystem #:cl-dag-consensus
  :name "cl-dag-consensus"
  :version "0.1.0"
  :author "Parkian Company LLC"
  :license "MIT"
  :description "DAG-based consensus protocol implementing GHOSTDAG for directed acyclic graph blockchains"
  :long-description "A standalone Common Lisp implementation of DAG-based consensus with:
- Directed acyclic graph block structure with multiple parents
- GHOSTDAG protocol for vertex coloring and ordering
- Heaviest branch selection for tip determination
- Merge block creation and validation
- Topological ordering and finality computation
- Transaction conflict resolution"

  :depends-on ()  ; Pure Common Lisp - no external dependencies

  :components
  ((:file "package")
   (:module "src"
    :depends-on ("package")
    :serial t
    :components
    ((:file "util")
     (:file "vertex")
     (:file "dag")
     (:file "ordering")
     (:file "consensus"))))

  :in-order-to ((test-op (test-op #:cl-dag-consensus/test))))

(defsystem #:cl-dag-consensus/test
  :name "cl-dag-consensus/test"
  :version "0.1.0"
  :license "MIT"
  :description "Tests for cl-dag-consensus"

  :depends-on (#:cl-dag-consensus)

  :components
  ((:module "test"
    :components
    ((:file "test-dag"))))

  :perform (test-op (op c)
             (let ((result (symbol-call :cl-dag-consensus.test :run-tests)))
               (unless result
                 (error "Tests failed")))))
