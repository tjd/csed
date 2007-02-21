(load "amb.scm")

(define gen-prime
  (lambda (hi)
    (let ((i (number-between 2 hi)))
      (assert (prime? i))
      i)))

(define gen-divisor
  (lambda (n)
    (let ((d (number-between 1 n)))
      (assert (divides d n))
      d)))

(define pyth-triple
  (lambda (n)
    (let* ((a (number-between 1 n))
           (b (number-between a n))
           (c (number-between b n)))
      (assert (= (* c c) (+ (* a a) (* b b))))
      (list a b c))))

(define bit
  (lambda ()
    (amb 0 1)))

(define n-bits
  (lambda (n)
    (if (<= n 0)
        '()
        (cons (bit) (n-bits (- n 1))))))

(define bit-strings
  (lambda ()
    (list (bit) (bit) (bit))))

(define n-digits
  (lambda (n)
    (if (<= n 0)
        '()
        (cons (number-between 1 9) (n-digits (sub1 n))))))

(define diff-pair
  (lambda ()
    (let* ((d1 (number-between 1 9))
           (d2 (number-between (+ d1 2) 9)))
      (list d1 d2))))