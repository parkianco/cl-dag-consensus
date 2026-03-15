;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-dag-consensus)

;;; Core types for cl-dag-consensus
(deftype cl-dag-consensus-id () '(unsigned-byte 64))
(deftype cl-dag-consensus-status () '(member :ready :active :error :shutdown))
