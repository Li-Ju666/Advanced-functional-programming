#lang racket

(provide new-graph add-edge add-vert kcolor)
(require rackunit)

;; implementation of graph
(define new-graph '())

(define (add-edge g e)
    (if (eq? (first e) (second e))
        (add-vert g (first e))
        (add-edge-b (add-edge-b g e) (reverse e))))
; helper function for add-edge
(define (add-edge-b g e)
    (match g
        ['() (list (list (first e) (list (second e))))]
        [(cons v vs) #:when (eq? (first v) (first e))
            (cons (list (first v) (remove-duplicates (cons (second e) (second v)))) vs)]
        [(cons v vs) (cons v (add-edge-b vs e))]))

(define (add-vert g v)
    (match g
        ['() (list (list v '()))]
        [(cons v1 vs) #:when (eq? (first v1) v) g]
        [(cons v1 vs) (cons v1 (add-vert vs v))]))

; get adjacent vertices of a vertex
(define (get-adjs g v)
    (match g
        ['() '()]
        [(cons v1 vs) #:when (eq? (first v1) v) (second v1)]
        [(cons v1 vs) (get-adjs vs v)]))

; get all vertices of a graph
(define (get-verts g)
    (match g
        ['() '()]
        [(cons v vs) (cons (first v) (get-verts vs))]))

; get all edges of a graph: redundant edges included: e.g. '(1 2) and '(2 1)
(define (get-edges g) (get-edges-a g '()))
; helper function for get-edges
(define (get-edges-a g acc)
    (match g
        ['() acc]
        [(cons v vs)
           (get-edges-a vs (append (for/list ([i (second v)]) (list (first v) i)) acc))]))

;; Implementation of kcolor
; get available colors
(define (get-avail-colors n)
    (take '(a b c d e f g h i j k l m n o p q r s t u v w x y z) n))

(define (kcolor g n) (kcolora g g '() (get-avail-colors n)))

(define (kcolora og g colored colors)
    (match g
        ['() (sort colored color-order?)]
        [(cons v1 vs)
           (let ([valid-coloring
                    (for/list ([x colors] #:when (is-valid og colored (list (first v1) x)))
                    (cons (list (first v1) x) colored))])
;             vs)]))
               (match valid-coloring
                   ['() #f]
                   [valid-coloring
                      (foldl
                         (lambda (x acc) (if acc acc (kcolora og vs x colors)))
                         #f valid-coloring)]))]))

(define (is-valid g colored color)
    (let ([adjs (get-adjs g (first color))])
          (foldl (lambda (x acc) (if (eq? (get-color x colored) (second color)) #f acc)) #t adjs)))

(define (get-color v colored)
    (match colored
        ['() 'null]
        [(cons c1 cs) #:when (eq? (first c1) v) (second c1)]
        [(cons c1 cs) (get-color v cs)]))

(define (color-order? c1 c2) (if (< (first c1) (first c2)) #t #f))

;; Unit tests
; graph with 3 vertices
(check-equal? (kcolor '((1 (2 3)) (2 (1 3)) (3 (1 2))) 3) '((1 a) (2 b) (3 c)) "check with 3 vertices")
(check-equal? (kcolor '((1 (2 3)) (2 (1 3)) (3 (1 2))) 2) #f "check with 3 vertices")

; graph with 4 vertices
(check-equal? (kcolor '((1 (2 3 4)) (2 (1 3)) (3 (1 2)) (4 (1))) 3)
              '((1 a) (2 b) (3 c) (4 b)) "check with 4 vertices")
(check-equal? (kcolor '((1 (2 3 4)) (2 (1 3)) (3 (1 2 4)) (4 (1 3))) 3)
              '((1 a) (2 b) (3 c) (4 b)) "check with 4 vertices")
(check-equal? (kcolor '((1 (2 3 4)) (2 (1 3 4)) (3 (1 2 4)) (4 (1 3 2))) 3)
              #f "check with 4 vertices")
(check-equal? (kcolor '((1 (2 3 4)) (2 (1 3 4)) (3 (1 2 4)) (4 (1 3 2))) 4)
              '((1 a) (2 b) (3 c) (4 d)) "check with 4 vertices")

; graph with 5 vertices
(check-equal? (kcolor '((1 (3 5)) (2 (4 5)) (3 (1 4)) (4 (2 3)) (5 (1 2))) 3)
              '((1 a) (2 a) (3 b) (4 c) (5 b)) "check with 5 vertices")
(check-equal? (kcolor '((1 (2 3)) (2 (1 4 5)) (3 (1 4 5)) (4 (2 3 5)) (5 (2 3 4))) 3)
              '((1 a) (2 b) (3 b) (4 a) (5 c)) "check with 5 vertices")
(check-equal? (kcolor '((1 (2 3)) (2 (1 4 5)) (3 (1 4 5)) (4 (2 3 5)) (5 (2 3 4))) 2)
              #f "check with 5 vertices")