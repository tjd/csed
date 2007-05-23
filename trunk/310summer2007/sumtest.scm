;;; sumtest.scm

;;; vanilla sum function
(define (sum lst)
 (if (null? lst)
     0
     (+ (car lst) (sum (cdr lst)))))

;;; global variable to collect all the information we want printed
(define collect '())

;;; puts x on the front of collect; x is returned
(define (remember x)
 (set! collect (cons x collect))
 x)

(define (sum-print lst)
 (set! collect '())
 (display-results (sum-print-aux lst) (reverse collect)))

(define (sum-print-aux lst)
 (if (null? lst)
     0
     (+ (remember (car lst)) (sum-print-aux (cdr lst)))))

;;; functions for printing the final results
(define (display-results result lst)
 (display (car lst))  ;;; the first item is printed differently than all the rest
 (display-results-aux (cdr lst))
 (display " = ")
 (display result))

(define (display-results-aux lst)
 (if (null? (cdr lst))
    (begin (display " + ")          ;;; begin lets you do a sequence of operations
           (display (car lst)))
    (begin (display " + ")
           (display (car lst))
           (display-results-aux (cdr lst)))))