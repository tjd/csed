;; helper methods for ?x-style variables

;; vars are atoms beginning with ?
(define (var? a)
  (if (symbol? a)
      (let ((s (symbol->string a)))
	(and (< 1 (string-length s))
	     (eq? #\? (string-ref s 0))))
      #f))

;; returns (as a symbol) a variable with the given name
(define (make-var sym)
  (string->symbol
   (string-append "?" (symbol->string sym))))

;; keeps count of number of variables
(define var-count 0)

;; returns a new variable of the form ?vi, where i is 
;; the value of var-count; each time (next-var) is
;; returned, a unique variable of the form ?vi is
;; returned, with the i-values in increasing order, 
;; e.g. ?v1, ?v2, ?v3, ...
(define (next-var)
  (begin
    (set! var-count (+ 1 var-count))
    (string->symbol 
     (string-append "?v" (number->string var-count)))))