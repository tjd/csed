(load "tools.scm")

(define divides
  (lambda (a b)
    (= 0 (modulo b a))))

(define get-lowest-divisor
  (lambda (n div)
    (if (divides div n)
	div
	(get-lowest-divisor n (add1 div)))))

(define divisors-aux
  (lambda (n div)
    (cond 
      ((< n div) '())
      ((= n div) (list div))
      (else
       (let ((ld (get-lowest-divisor n div)))
         (cons ld (divisors-aux n (add1 ld)))
         )))))

(define divisors (lambda (n) (divisors-aux n 1)))
(define d (lambda (n) (length (divisors n))))
(define prime? (lambda (n) (= 2 (d n))))
