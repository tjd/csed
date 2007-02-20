;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; amb, from http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-16.html#node_idx_470
(require (lib "defmacro.ss"))
(load "tools.scm")

(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
      (lambda ()
        (error "amb tree exhausted")))))

(initialize-amb-fail)

(define-macro amb
  (lambda alts...
    `(let ((+prev-amb-fail amb-fail))
       (call/cc
        (lambda (+sk)

          ,@(map (lambda (alt)
                   `(call/cc
                     (lambda (+fk)
                       (set! amb-fail
                         (lambda ()
                           (set! amb-fail +prev-amb-fail)
                           (+fk 'fail)))
                       (+sk ,alt))))
                 alts...)

          (+prev-amb-fail))))))

(define-macro bag-of
  (lambda (e)
    `(let ((+prev-amb-fail amb-fail)
           (+results '()))
       (if (call/cc
            (lambda (+k)
              (set! amb-fail (lambda () (+k #f)))
              (let ((+v ,e))
                (set! +results (cons +v +results))
                (+k #t))))
           (amb-fail))
       (set! amb-fail +prev-amb-fail)
       (reverse! +results))))

;; Non-deterministically returns a number from lo to hi, inclusive.
(define number-between
  (lambda (lo hi)
    (let loop ((i lo))
      (if (> i hi) (amb)
          (amb i (loop (+ i 1)))))))

;; Non-deterministically returns an element of lst.
(define element-of
  (lambda (lst)
    (if (singleton? lst)
        (car lst)
        (amb (car lst) (element-of (cdr lst))))))

(define assert
  (lambda (pred)
    (if (not pred) (amb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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