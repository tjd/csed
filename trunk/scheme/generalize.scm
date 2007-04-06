(load "tools.scm")
(load "variable.scm")

(define (ground? a)
  (and (atom? a) (not (var? a))))

;; predicates have the form (pred arg1 arg2 ...)
;; the args can be variables or atoms
(define (pred? a)
  (and (list? a)
       (atom? (car a))
       (all? (cdr a) (lambda (x) (or (var? x) (atom? x))))))

;; a ground predicate has no variable arguments
(define (ground-pred? a)
  (and (list? a)
       (atom? (car a))
       (all? (cdr a) ground?)))

;; replace x with a variable if it is ground; otherwise return x
;; unchanged
(define (replace-if-ground x)
  (if (ground? x)
      (next-var)
      x))

;; assumes lst contains only variables and atoms
(define (abstract-all-ground lst)
  (if (null? lst) 
      '()
      (cons (if (ground? (car lst))
		(next-var)
		(car lst))
	    (abstract-all-ground (cdr lst)))))
	 

;; returns a list of predicates with exactly one non-variable
;; parameter replaced with a variable
(define (abstract-all-param pred)
  (map (lambda (x) (cons (car pred) x))
       (abstract-all-ground (cdr pred))))


   