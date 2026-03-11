;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; package.lisp - Package definition for cl-dag-consensus
;;;; SPDX-License-Identifier: MIT

(defpackage #:cl-dag-consensus
  (:use #:cl)
  (:nicknames #:dag-consensus)
  (:documentation "DAG-based consensus protocol implementing GHOSTDAG.

Provides directed acyclic graph block structure with multiple parents,
heaviest branch selection, merge block validation, and finality computation.")

  (:export
   ;; =========================================================================
   ;; Constants
   ;; =========================================================================
   #:+max-parents+
   #:+min-parents+
   #:+default-k-param+
   #:+merge-threshold+
   #:+finality-threshold+

   ;; =========================================================================
   ;; Utility Functions
   ;; =========================================================================
   #:zero-hash
   #:hash-equal-p
   #:copy-hash
   #:hash-to-hex
   #:hex-to-hash

   ;; =========================================================================
   ;; Vertex Types and Operations
   ;; =========================================================================

   ;; DAG Vertex
   #:dag-vertex
   #:make-dag-vertex
   #:dag-vertex-p
   #:dag-vertex-id
   #:dag-vertex-hash
   #:dag-vertex-height
   #:dag-vertex-timestamp
   #:dag-vertex-work
   #:dag-vertex-cumulative-work
   #:dag-vertex-parents
   #:dag-vertex-children
   #:dag-vertex-data
   #:dag-vertex-color
   #:dag-vertex-score
   #:dag-vertex-is-merge-p

   ;; DAG Edge
   #:dag-edge
   #:make-dag-edge
   #:dag-edge-p
   #:dag-edge-from
   #:dag-edge-to
   #:dag-edge-weight
   #:dag-edge-type

   ;; DAG Block
   #:dag-block
   #:make-dag-block
   #:dag-block-p
   #:dag-block-version
   #:dag-block-parent-hashes
   #:dag-block-merkle-root
   #:dag-block-timestamp
   #:dag-block-bits
   #:dag-block-nonce
   #:dag-block-blue-score
   #:dag-block-transactions
   #:dag-block-merge-proof
   #:dag-block-header
   #:dag-block-hash
   #:is-genesis-block-p
   #:parent-count
   #:is-multi-parent-p

   ;; Merge Proof
   #:merge-proof
   #:make-merge-proof
   #:merge-proof-p
   #:merge-proof-branches
   #:merge-proof-resolution
   #:merge-proof-conflict-set
   #:merge-proof-ancestor-hash
   #:merge-proof-work-proofs
   #:merge-proof-timestamp

   ;; Conflict Resolution
   #:conflict-resolution
   #:make-conflict-resolution
   #:conflict-resolution-p
   #:conflict-resolution-conflict-id
   #:conflict-resolution-tx-accepted
   #:conflict-resolution-tx-rejected
   #:conflict-resolution-reason
   #:conflict-resolution-branch-hash

   ;; Branch Info
   #:branch-info
   #:make-branch-info
   #:branch-info-tip-hash
   #:branch-info-ancestor-hash
   #:branch-info-height
   #:branch-info-cumulative-work

   ;; Validation Result
   #:dag-validation-result
   #:make-dag-validation-result
   #:dag-validation-result-valid-p
   #:dag-validation-result-errors
   #:dag-validation-result-warnings

   ;; GHOSTDAG Data
   #:ghostdag-data
   #:make-ghostdag-data
   #:ghostdag-data-blue-score
   #:ghostdag-data-blue-work
   #:ghostdag-data-selected-parent

   ;; =========================================================================
   ;; DAG Structure
   ;; =========================================================================

   ;; DAG Consensus object
   #:dag-consensus
   #:make-dag-consensus
   #:dag-consensus-p
   #:dag-consensus-genesis
   #:dag-consensus-vertices
   #:dag-consensus-tips
   #:dag-consensus-finalized
   #:dag-consensus-k-param

   ;; DAG Construction
   #:add-vertex
   #:add-edge
   #:remove-vertex
   #:connect-parents

   ;; DAG Queries
   #:get-vertex
   #:get-vertices
   #:get-parents
   #:get-children
   #:get-ancestors
   #:get-descendants
   #:get-tips
   #:get-genesis
   #:vertex-count
   #:edge-count

   ;; DAG Properties
   #:is-ancestor-p
   #:is-descendant-p
   #:is-tip-p
   #:is-orphan-p
   #:is-valid-dag-p
   #:has-cycle-p
   #:reachable-p

   ;; =========================================================================
   ;; Topological Operations
   ;; =========================================================================

   #:topological-sort
   #:reverse-topological-sort
   #:compute-block-order
   #:transaction-total-order

   ;; =========================================================================
   ;; Heaviest Branch Selection
   ;; =========================================================================

   ;; Weight Calculation
   #:calculate-vertex-weight
   #:calculate-branch-weight
   #:calculate-cumulative-work
   #:update-weights

   ;; Tip Selection
   #:select-best-tip
   #:select-best-tips
   #:rank-tips-by-weight
   #:tip-selection-strategy

   ;; Branch Analysis
   #:find-heaviest-branch
   #:compare-branches
   #:branch-work-ratio
   #:branch-depth

   ;; GHOSTDAG
   #:calculate-blue-score
   #:get-blue-set
   #:get-red-set
   #:get-anticone
   #:ghostdag-ordering

   ;; =========================================================================
   ;; Finality
   ;; =========================================================================

   #:compute-finality-score
   #:is-final-p
   #:finality-depth
   #:confirmation-count

   ;; =========================================================================
   ;; Merge Block Operations
   ;; =========================================================================

   ;; Merge Detection
   #:detect-merge-candidates
   #:is-merge-block-p
   #:find-divergence-point
   #:find-convergence-points

   ;; Merge Validation
   #:validate-merge-block
   #:validate-merge-proof
   #:check-merge-conflicts
   #:verify-merge-ancestry

   ;; Merge Block Creation
   #:create-merge-block
   #:compute-merge-proof
   #:select-merge-parents
   #:resolve-transaction-conflicts

   ;; Conflict Resolution
   #:find-conflicting-transactions
   #:resolve-double-spends
   #:determine-canonical-tx
   #:conflict-resolution-strategy

   ;; =========================================================================
   ;; Serialization
   ;; =========================================================================

   #:serialize-dag
   #:deserialize-dag
   #:serialize-vertex
   #:deserialize-vertex

   ;; =========================================================================
   ;; Validation
   ;; =========================================================================

   #:validate-dag-block
   #:validate-dag-structure
   #:verify-acyclicity

   ;; =========================================================================
   ;; Statistics and Display
   ;; =========================================================================

   #:dag-statistics
   #:avg-parents-per-block
   #:dag-width
   #:dag-depth
   #:merge-statistics
   #:print-dag-summary
   #:print-vertex-info
   #:print-tip-selection

   ;; =========================================================================
   ;; Event Hooks
   ;; =========================================================================

   #:*on-vertex-added*
   #:*on-tip-changed*
   #:*on-merge-detected*
   #:*on-finality-reached*))

(defpackage #:cl-dag-consensus.test
  (:use #:cl #:cl-dag-consensus)
  (:export #:run-tests))
