#lang racket

(require racket/set)

(define-namespace-anchor sql)
(define lite-sql (namespace-anchor->namespace sql))

; each relation is a pair: the first element is a list of pairs, each pair is consisted of
; column name and its type; and the second element is a hash set, storing all records
(define-syntax-rule (CREATE tname cols)
  (define tname (list (header-generate 'cols) (set))))

; helper function to generate header of a relation
(define (header-generate cols)
  (map (lambda (x)
         (list (first x) (string->symbol (string-append (symbol->string (second x)) "?")))
         ) cols))

(define-syntax INSERT
  (syntax-rules (INTO VALUES)
    [(INSERT INTO tname VALUES other ...)
     (let ((records (list 'other ...)))
       (if (checkValidity tname records)
           (set! tname (list (first tname) (set-union (second tname) (list->set records))))
           (error "Invalid input")))]))


(define (checkValidity table records)
  (and
    ; check type
    (let ((types? (flatten (make-list (length records) (map second (first table))))))
      (eval (cons 'and
            (map (lambda (type? value) (eval (list type? value) lite-sql))
                 types? (flatten records))) lite-sql))
    ; check length
    (eval (cons 'and
                (map (lambda (record) (eq? (length (first table)) (length record)))
                     records)) lite-sql)))


(define-syntax SELECT
  (syntax-rules (FROM WHERE)
    [(SELECT cols FROM table)
     (pickCols (getColIndices 'cols table) (set->list (second table)))]
;    [(SELECT cols FROM table WHERE conditions)
;     (pickCols (first table) '???)]
))

(define (getColIndices cols table)
  (cond [(eq? cols '*) (range 0 (length (first table)) 1)]
        [(list? cols) (map (lambda (colname) (index-of (map first (first table)) colname)) cols)]
        [else (list (index-of (map first (first table)) cols))]))

(define (pickCols indices records)
  (map (lambda (record) (map (lambda (index) (list-ref record index)) indices)) records))


;; TEST
(CREATE mytable ((department integer) (dept_name string)))
(display mytable)
(display "\n")
(INSERT INTO mytable VALUES
        (1 "Sales")
        (2 "Marketing"))
(display mytable)
(display "\n")

(INSERT INTO mytable VALUES
        (1 "Sales")
        (2 "Marketing"))

(display mytable)
(display "\n")

(INSERT INTO mytable VALUES
        (3 "adasdf")
        (4 "asdf"))

(display mytable)
(display "\n")
(CREATE employees
((emp_name string) (department integer) (salary integer) (hired integer)))
(display employees)
(display "\n")
(INSERT INTO employees VALUES
          ("John" 1 1000 2003)
          ("Jack" 2 2000 2000)
          ("James" 3 1000 2010))
(display employees)
(display "\n")
(SELECT (emp_name salary department) FROM employees)
(SELECT emp_name FROM employees)
(SELECT * FROM employees)