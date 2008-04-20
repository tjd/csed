(load "amb.scm")
(load "unify.scm")

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

;; For each 1-bit in bits, keep the item at the corresponding position in bits, e.g.
;; > (keep-if-1 '(0 1 0 0 1) '(a b c d e))
;; (b e)
(define (keep-if-1 bits lst)
  (cond ((null? bits) '())
	((= 0 (car bits)) (keep-if-1 (cdr bits) (cdr lst)))
	((= 1 (car bits)) (cons (car lst) (keep-if-1 (cdr bits) (cdr lst))))))

;;; generates all subsets of lst
(define (subset-of lst)
  (let ((bits (n-bits (length lst))))
    (keep-if-1 bits lst)))

;;; generates all subsets of lst
(define (subset-of2 lst)
  (if (null? lst)
      '()
      (let ((sub (subset-of2 (cdr lst))))
        (amb sub (cons (car lst) sub)))))

;; returns all pairs of the Cartesian product of A and B
(define (prod-of A B)
  (list (element-of A) (element-of B)))

;; returns tuples of the lists in lst
(define (prod-all-of lst)
  (if (null? lst)
      '()
      (cons (element-of (car lst)) (prod-all-of (cdr lst)))))

;; generate pairs of elements of lst
(define (pair-of lst)
  (if (< (length lst) 2)
      (amb)
      (amb (list (car lst) (element-of (cdr lst)))
           (pair-of (cdr lst)))))
           
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

(define (uab1)
  (assert (unify (list 1 2 (amb 'a 'b)) (list 1 2 'b))))