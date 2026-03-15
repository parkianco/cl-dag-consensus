;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(asdf:defsystem #:cl-dag-consensus
  :description "DAG-based consensus protocol implementing GHOSTDAG for directed acyclic graph blockchains"
  :author "Park Ian Co"
  :license "Apache-2.0"
  :version "0.1.0"
  :serial t
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "conditions")
             (:file "types")
             (:file "util")
             (:file "vertex")
             (:file "dag")
             (:file "ordering")
             (:file "consensus")
             (:file "cl-dag-consensus"))))
  :in-order-to ((asdf:test-op (test-op #:cl-dag-consensus/test))))

(asdf:defsystem #:cl-dag-consensus/test
  :description "Tests for cl-dag-consensus"
  :author "Park Ian Co"
  :license "Apache-2.0"
  :depends-on (#:cl-dag-consensus)
  :serial t
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "package")
     (:file "test"))))
  :perform (asdf:test-op (o c)
             (let ((result (uiop:symbol-call :cl-dag-consensus.test :run-tests)))
               (unless result
                 (error "Tests failed")))))
