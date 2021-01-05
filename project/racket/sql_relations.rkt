#lang racket

(require racket/set)

(define-namespace-anchor sql)
(define lite-sql (namespace-anchor->namespace sql))

; each relation is a pair: the first element is a list of pairs, each pair is consisted of
; column name and its type; and the second element is a hash set, storing all records

; ------------------------------------- CREATE -------------------------------------
; macro to match CREATE statement
(define-syntax-rule (CREATE tname cols)
  (define tname (list (header-generate 'cols) (set))))

; helper function to generate header of a relation
(define (header-generate cols)
  (map (lambda (x colNum)
         (list (first x)
               (string->symbol (string-append (symbol->string (second x)) "?"))
               colNum)
         ) cols (range 0 (length cols) 1)))

; ------------------------------------- INSERT -------------------------------------
; macro to match INSERT statement
(define-syntax INSERT
  (syntax-rules (INTO VALUES)
    [(INSERT INTO tname VALUES other ...)
     (let ((records (list 'other ...)))
       (if (checkValidity tname records)
           (set! tname (list (first tname) (set-union (second tname) (list->set records))))
           (error "Invalid input")))]))

; function to check if input records are of proper type and same length as table headers
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


; ---------------------------------- SELECT ---------------------------------------
; macro to match SELECT statement
(define-syntax SELECT
  (syntax-rules (FROM WHERE AND OR)
    ; SELECT from table name with parantheses
    [(SELECT cols FROM (table) others ...)
     (SELECT cols FROM table others ...)]
    ; SELECT with AND: condition1 is wrapped with parantheses
    [(SELECT cols FROM table WHERE condition1 AND conditions ...)
     (set->list (set-intersect
      (list->set (SELECT cols FROM table WHERE condition1))
      (list->set (SELECT cols FROM table WHERE conditions ...))))]
    ; SELECT with OR: condition1 is wrapped with parantheses
    [(SELECT cols FROM table WHERE condition1 OR conditions ...)
     (set->list (set-union
      (list->set (SELECT cols FROM table WHERE condition1))
      (list->set (SELECT cols FROM table WHERE conditions ...))))]
    ; Basic SELECT with one constraint without parantheses
    [(SELECT cols FROM table WHERE exp1 op exp2)
     (let ((result (getRecords '(exp1 op exp2) table)))
       (pickCols (getColIndices 'cols table) (set->list result)))]
    ; Handle unwrapped AND/OR conditions
    [(SELECT cols FROM table WHERE exp1 op exp2 others ...)
     (SELECT cols FROM table WHERE (exp1 op exp2) others ...)]
    ; Unwrap wrapped conditions
    [(SELECT cols FROM table WHERE (conditions ...))
     (SELECT cols FROM table WHERE conditions ...)]
    ; SELECT without constraints
    [(SELECT cols FROM table)
     (pickCols (getColIndices 'cols table) (set->list (second table)))]
))

;; function to get records which satisfy one condition: a list will be returned
; (define (getRecords condition table)
;   (match condition [(list exp1 op exp2) 
;     (let ((index (index-of (map first (first table)) exp1))
;           (value (eval exp2 lite-sql)))
;        (filter (lambda (record)
;                (; to decide operation: if = -> eq? if != -> not eq?, others keep same
;                 (cond
;                   [(eq? op '=) eq?]
;                   [(eq? op '!=) (lambda (x y) (not (eq? x y)))]
;                   [else (eval op lite-sql)])
;                 ; target value to be compared
;                 (list-ref record index)
;                 ; given value
;                 value))
;             (set->list (second table))))]))

; function to get records which satisfy one condition: a list will be returned
 (define (getRecords condition table)
   (match condition [(list exp1 op exp2) 
     (let ((index1 (index-of (map first (first table)) exp1))
           (index2 (index-of (map first (first table)) exp2))
           (value1 (with-handlers ([exn:fail? (lambda (exn) #f)]) (eval exp1 lite-sql)))
           (value2 (with-handlers ([exn:fail? (lambda (exn) #f)]) (eval exp2 lite-sql))))
        (filter (lambda (record)
                (; to decide operation: if = -> eq? if != -> not eq?, others keep same
                 (cond
                   [(eq? op '=) eq?]
                   [(eq? op '!=) (lambda (x y) (not (eq? x y)))]
                   [else (eval op lite-sql)])
                 ; target value to be compared
                 (if index1 (list-ref record index1) value1)
                 (if index2 (list-ref record index2) value2)))
             (set->list (second table))))]))

; function to get column indices of a list columns
(define (getColIndices cols table)
  (cond [(eq? cols '*) ; * to select all columns
         (range 0 (length (first table)) 1)]
        [(list? cols) ; to select a list of columns embraced by paranthesis
         (map (lambda (colname) (index-of (map first (first table)) colname)) cols)]
        [else ; to select a single column
         (list (index-of (map first (first table)) cols))]))

; function to select columns from an entire relation with column indices
(define (pickCols indices records)
  (map (lambda (record) (map (lambda (index) (list-ref record index)) indices)) records))



; ------------------------------ DELETE -------------------------------------
(define-syntax DELETE
  (syntax-rules (FROM WHERE)
    [(DELETE FROM table WHERE conditions ...)
     (set! table (list (first table)
           ; recursively remove records from original table
           (foldl (lambda (x acc) (set-remove acc x))
                  (second table)
                  ; select all records satisfying the condition
                  (SELECT * FROM table WHERE conditions ...))))]))


;(define-syntax ADD
;  (syntax-rules (MIN)
;    [(ADD a MIN b ...) '(b ...)]))

(define-syntax ADD
  (syntax-rules(MAX)
    [(ADD a b MAX(c)) c]))

;; --------------------------------------------------------------------------------
;; TEST
(CREATE employees
((emp_name string) (department integer) (salary integer) (hired integer)))

(SELECT * FROM employees)

(INSERT INTO employees VALUES
          ("John" 1 1000 2003)
          ("Jack" 2 2000 2000)
          ("James" 3 1000 2010)
          ("Tom" 4 2000 2009)
          ("Jonason" 5 1500 2008)
          ("Hanna" 6 1800 2012))

(SELECT * FROM employees)
(SELECT department FROM employees)
(SELECT (department) FROM employees)
(SELECT (department) FROM (employees))
(SELECT (department emp_name) FROM employees)


;(SELECT (emp_name salary department) FROM employees)
;(SELECT emp_name FROM employees)

;(SELECT (emp_name) FROM employees WHERE emp_name = "James")
;(SELECT (emp_name) FROM employees WHERE (emp_name = "James"))
;(SELECT (emp_name) FROM employees WHERE (department < 5) AND (department > 2))
;(SELECT * FROM employees WHERE department >= 2 AND department <= 5)
;        OR emp_name = "John")

;(SELECT (department emp_name) FROM employees
;        WHERE (department >= 1) AND (department < (* 1 3)))

;(SELECT department FROM employees WHERE (department = 2))
;(SELECT * FROM employees WHERE department = 2)
;(SELECT * FROM employees WHERE (department = (+ 1 1)))


;(DELETE FROM employees WHERE department = 2)
;(SELECT * FROM employees)


;(SELECT * FROM employees WHERE (department >= 2) AND department <= 5)
;(SELECT * FROM employees WHERE ((+ 1 2) >= 2 AND department <= 5) OR (hired < 2005))
;(DELETE FROM employees WHERE (department >= 2) AND department <= 5)
;(SELECT * FROM employees)
