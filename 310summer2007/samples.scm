;;; factorial function
(define (! n)
  (if (= n 0)
      1
      (* n (! (- n 1)))))


(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

;(define (sum lst)
;  (apply + lst))

(define (divide-all-by n lst)
  (map (lambda (x) (/ x n)) lst))
   
;(define (divide-all-by n lst)
;  (if (null? lst)
;      '()
;      (cons (/ (car lst) n)
;            (divide-all-by n (cdr lst)))))

(define (normalize lst)
  (divide-all-by (sum lst) lst))

(define (norm lst)
  (let ((sum (apply + lst)))
    (map (lambda (x) (/ x sum)) lst)))

(define (mymap fn lst)
  (if (null? lst)
      '()
      (cons (fn (car lst)) (mymap fn (cdr lst)))))

;; linear function
(define (speed1 t)
  t)

;; step function
(define (speed2 t)
  (if (<= t 0)
      0
      1))

;;; sigmoid function
(define (speed3 t)
  (/ 1.0 (+ 1 (exp (- t)))))

;;; returns a function that caluculates mx + b, e.g.
;;; > (define f1 (make-linear 10 -2))
;;; > (f1 5)
;;; 48
(define (make-linear m b)
  (lambda (x)
    (+ (* m x) b)))


;;; An arithmetic expression is defined recursively follows:
;;;   - All numbers are arithmetic expressions.
;;;   - If e1 and e2 are arithmetic expressions, then so are (e1 + e2), (e1 - e2),
;;;     (e1 * e2), and (e1 / e2).
;;;
;;; cadr and caddr return the second and third elements of a list respectively.
(define (eval-arith1 e)
  (cond ((number? e) e)
        ((eq? (cadr e) '+) (+ (eval-arith1 (car e))
                              (eval-arith1 (caddr e))))
        ((eq? (cadr e) '-) (- (eval-arith1 (car e))
                              (eval-arith1 (caddr e))))
        ((eq? (cadr e) '*) (* (eval-arith1 (car e))
                              (eval-arith1 (caddr e))))
        ((eq? (cadr e) '/) (/ (eval-arith1 (car e))
                              (eval-arith1 (caddr e))))))

;;; same as eval-arith1, but uses a let environment and the eval function
;;; to avoid repeated code
(define (eval-arith2 e)
  (cond ((number? e) e)
        (else (let ((op (cadr e))
                    (left (eval-arith2 (car e)))
                    (right (eval-arith2 (caddr e))))
                (eval (list op left right))))))

;;; another version, without using eval
(define (eval-arith3 e)
  (if (number? e)
      e
      (let ((op (cadr e))
            (left (eval-arith3 (car e)))
            (right (eval-arith3 (caddr e))))
        (cond ((eq? op '+) (+ left right))
              ((eq? op '*) (* left right))
              ((eq? op '-) (- left right))
              ((eq? op '/) (/ left right))))))

;;; This version allows variables in the expression. The parameter env is an association
;;; list of (symbol, value) pairs, and symbols are replaced with their corresponding 
;;; values in the evaluation.
;;; e.g.
;;; > (eval-arith4 '((a + 2) * (b + 4)) '((a 1) (b 2) (c 3)))
;;; 18
(define (eval-arith4 e env)
  (cond ((number? e) e)
        ((symbol? e) (cadr (assoc e env)))
        (else 
         (let ((op (cadr e))
               (left (eval-arith4 (car e) env))
               (right (eval-arith4 (caddr e) env)))
           (cond ((eq? op '+) (+ left right))
                 ((eq? op '*) (* left right))
                 ((eq? op '-) (- left right))
                 ((eq? op '/) (/ left right)))))))

;;; Pythagorean triples using the amb operator
(load "amb.scm")

(define (pyth-triple n)
    (let* ((a (number-between 1 n))
           (b (number-between a n))
           (c (number-between b n)))
      (assert (= (* c c) (+ (* a a) (* b b))))
      (list a b c)))

;;; Generating bits strings with the amb operator
(define (bit)
    (amb 0 1))

(define (n-bits n)
    (if (<= n 0)
        '()
        (cons (bit) (n-bits (- n 1)))))

