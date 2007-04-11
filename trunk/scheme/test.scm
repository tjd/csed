
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

