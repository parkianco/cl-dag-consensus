;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-dag-consensus)

(define-condition cl-dag-consensus-error (error)
  ((message :initarg :message :reader cl-dag-consensus-error-message))
  (:report (lambda (condition stream)
             (format stream "cl-dag-consensus error: ~A" (cl-dag-consensus-error-message condition)))))
