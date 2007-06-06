;;; returns true just when every item on lst satisfies test?
(define (all? test? lst)
  (if (null? lst) 
      #t
      (and (test? (car lst)) (all? test? (cdr lst)))))

;;; return a list containing just those elements that satisfy the test function
(define (keep-if test-fn lst)
  (cond ((null? lst) '())
        ((test-fn (car lst)) (cons (car lst) (keep-if test-fn (cdr lst))))
        (else (keep-if test-fn (cdr lst)))))

;;;
;;; Horn clause recognizer functions
;;; 
(define (fact? x)
  (and (symbol? x) (not (eq? x '<--))))

(define (integrity-constraint? x)
  (and (list? x)
       (< 1 (length x))
       (all? symbol? x)))

(define (definite-clause? x)
  (and (list? x)
       (<= 3 (length x))
       (symbol? (car x))
       (eq? '<-- (cadr x))
       (all? symbol? (cddr x))))

(define (horn-clause? x)
  (or (fact? x)
      (integrity-constraint? x)
      (definite-clause? x)))

;;; definite clause helper functions
(define (head dc)
  (car dc))

(define (body dc)
  (cddr dc))

(define (premise dc)
  (head dc))

(define (conclusion dc)
  (body dc))


;;; sample KB: single fact
(define KB-wet
  '(wet))

;;; sample KB: four facts and no clauses
(define KB-pets
  '(cat dog fish moose))

;;; sample KB: one fact and one clause
(define KB-awake
  '(alarm
    (awake <-- alarm)))

;;; sample KB (from p. 220 of Norvig & Russell)
(define KB-nr
  '((Q <-- P)
    (P <-- L M)
    (M <-- B L)
    (L <-- A P)
    (L <-- A B)
    A
    B))

(define KB-figure-eight
  '((C <-- G)
    (G <-- F)
    (F <-- E)
    (E <-- D)
    (D <-- C)
    (C <-- B)
    (B <-- A)
    (A <-- H)
    (H <-- C)
    F))

(define KB-misc
  '((X <-- A B C)
    (X <-- D)
    (D <-- A B C)
    (A <-- B)
    (A <-- C)
    B
    C))

