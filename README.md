# cl-dag-consensus

DAG-based consensus protocol implementing GHOSTDAG with **zero external dependencies**.

## Features

- **GHOSTDAG**: Greedy Heaviest Observed SubTree DAG consensus
- **Parallel blocks**: Multiple blocks at same height
- **Blue/red scoring**: Block ordering and coloring
- **Phantom protocol**: Kaspa-style PHANTOM consensus
- **Pure Common Lisp**: No CFFI, no external libraries

## Installation

```lisp
(asdf:load-system :cl-dag-consensus)
```

## Quick Start

```lisp
(use-package :cl-dag-consensus)

;; Create DAG consensus
(let ((dag (make-ghostdag :k 18)))  ; k=18 for Kaspa
  ;; Add block
  (dag-add-block dag
                 :hash *block-hash*
                 :parents *parent-hashes*)
  ;; Get blue score
  (dag-blue-score dag *block-hash*)
  ;; Get selected parent
  (dag-selected-parent dag *block-hash*))
```

## API Reference

### DAG Operations

- `(make-ghostdag &key k)` - Create GHOSTDAG with parameter k
- `(dag-add-block dag &key hash parents)` - Add block to DAG
- `(dag-blue-score dag hash)` - Get blue score
- `(dag-selected-parent dag hash)` - Get selected parent chain
- `(dag-blue-set dag hash)` - Get blue set of block
- `(dag-is-blue-p dag hash)` - Check if block is blue

### Ordering

- `(dag-topological-order dag)` - Get topological order
- `(dag-merge-set dag hash)` - Get merge set for block

## Testing

```lisp
(asdf:test-system :cl-dag-consensus)
```

## License

BSD-3-Clause

Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
