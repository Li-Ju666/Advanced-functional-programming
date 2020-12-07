#lang racket
(require rackunit)
(provide perm)

(define (perm L1 L2) 
    (perms (reverse L1) (reverse L2) '()))

(define (perms L1 L2 stack)
    (match L1 
        ['()
            (match L2
                ['() #t]
                [(cons o os) #:when (eq? o (first stack)) (perms '() os (rest stack))]
                [else #f])]
        [L1
             (match stack
                 [(cons s ss) #:when (eq? s (first L2)) (perms L1 (rest L2) ss)]
                 [stack #:when (eq? (first L1) (first L2)) (perms (rest L1) (rest L2) stack)]
                 [else (perms (rest L1) L2 (cons (first L1) stack))])]))


; Unit test
(check-equal? (perm '(1 2 3) '(2 1 3)) #t "check with 3 elements")
(check-equal? (perm '(1 2 3) '(2 3 1)) #f "check with 3 elements")

(check-equal? (perm '(1 2 3 4) '(2 1 4 3)) #t "check with 4 elements")
(check-equal? (perm '(1 4 3 2) '(3 1 2 4)) #f "check with 4 elements")

(check-equal? (perm '(4 5 3 2 1) '(4 5 1 3 2)) #t "check with 5 elements")
(check-equal? (perm '(3 1 4 2 5) '(2 1 5 4 3)) #f "check with 5 elements")

(check-equal? (perm '(1 8 3 10 2 5 7 6 4 9) '(4 1 3 8 10 6 7 5 2 9)) #t "check with 10 elements")
(check-equal? (perm '(8 3 9 7 2 6 4 1 10 5) '(9 2 8 5 1 4 6 7 3 10)) #f "check with 10 elements")

