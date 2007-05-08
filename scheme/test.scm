
(define var?
  (lambda (a)
    (let ((s (symbol->string a)))
      (and (< 1 (string-length s))
	   (eq? #\? (string-ref s 0))))))

(define make-var
  (lambda (a)
    (string->symbol (string-append "?" (symbol->string a)))))

;; returns a function that takes a single list as a 
;; parameter and returns true if target is in the list,
;; and false otherwise
;(define (make-search target)
;  (lambda (lst)
;    (member target lst)))

(define make-search
  (lambda (target)
    (lambda (lst)
      (member target lst))))

(define add
  (lambda (x)
    (lambda (y)
      (+ x y))))

(define (! n)
  (if (= n 0)
      1
      (* n (! (- n 1)))))

;(define (sum lst)
;  (if (null? lst)
;      0
;      (+ (car lst) (sum (cdr lst)))))

(define (sum lst)
  (apply + lst))

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


