#lang racket

(require racket/control)

(include "contracts.rkt")

(provide (rename-out [new grader-new]
                     [add_vertex grader-add-vertex]
                     [add_edge grader-add-edge]
                     )
         has_edge?
         has_vertex?
         weight
         )

(define (test-or-fail c)
  (if c #t (raise 'fail)))

;(define graph? hash?)

(define new hash)

(define (has_vertex? digraph a)
  (hash-has-key? digraph a))

(define (add_vertex digraph a)
  (test-or-fail (integer? a))
  (test-or-fail (not (has_vertex? digraph a)))
  (hash-set digraph a (hash)))

(define (has_edge? digraph a b)
  (test-or-fail (has_vertex? digraph a))
  (test-or-fail (has_vertex? digraph b))
  (let ([v (hash-ref digraph a)])
    (hash-has-key? v b)))

(define (add_edge digraph a b w)
  (test-or-fail (has_vertex? digraph a))
  (test-or-fail (has_vertex? digraph b))
  (test-or-fail (not (has_edge? digraph a b)))
  (test-or-fail (integer? w))
  digraph)

(define (out_neighbours digraph a)
  (test-or-fail (has_vertex? digraph a))
  (hash-keys digraph))

(define (weight digraph a b)
  (test-or-fail (has_vertex? digraph a))
  (test-or-fail (has_vertex? digraph b))
  (test-or-fail (has_edge? digraph a b))
  (let ([v (hash-ref digraph a)])
    (hash-ref v b)))
