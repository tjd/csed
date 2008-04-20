;;; compose.scm

(define (neg? n)
  (< n 0))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (neg-fn fn)
  (compose not fn))

(define (and-fn f1 f2)
  (lambda (x) (and (f1 x) (f2 x))))

(define (or-fn f1 f2)
  (lambda (x) (or (f1 x) (f2 x))))

(define (remove-if pred lst)
  (cond ((null? lst) '())
	((pred (car lst)) (remove-if pred (cdr lst)))
	(else (cons (car lst) (remove-if pred (cdr lst))))))
      
(define (keep-if pred lst)
  (remove-if (neg-fn pred) lst))
