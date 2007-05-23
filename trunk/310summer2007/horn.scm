;;; returns true just when every item on lst satisfies test?
(define (all? test? lst)
  (if (null? lst) 
      #t
      (and (test? (car lst)) (all? test? (cdr lst)))))

;;;
;;; Horn clause recognizer functions
;;; 
(define (fact? x)
  (and (list? x)
       (= 1 (length x))
       (symbol? (car x))))

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