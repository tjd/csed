;;; breadth first search
(require (lib "trace.ss"))  ;; used for debugging
(load "tools.scm")

(define (bfs goal-fn kids-fn open closed)
    (cond ((null? open) 'FAIL)
          ((goal-fn (car open)) 'SUCCESS)
          (else
           (let ((x (car open)))
             (bfs goal-fn kids-fn 
                  (append (cdr open) (filter-kids x kids-fn open closed)) ;; new open list
                  (cons x closed))))))

(define (filter-kids x kids-fn open closed)
  (let ((kids (kids-fn x)))
    (if (null? kids)
        kids
        (remove-if (lambda (a) (or (memq a open) (memq a closed))) kids))))

;;;; helper functions
;;;; return a list containing just those elements that satisfy the test function
;(define (keep-if test-fn lst)
;  (cond ((null? lst) '())
;        ((test-fn (car lst)) (cons (car lst) (keep-if test-fn (cdr lst))))
;        (else (keep-if test-fn (cdr lst)))))
;
;(define (remove-if test-fn lst)
;  (keep-if (lambda (x) (not (test-fn x))) lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sample data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tree1 
  '((a (b c d))
    (b ())
    (c (e f))
    (d (g h))
    (e (i j))
    (f (k))
    (g (l m))
    (h ())
    (i ())
    (j ())
    (k ())
    (l ())
    (m ())))

;; return the children of a given node
(define (kids1 node)
  (cadr (assoc node tree1)))

;;; search for node "m" starting at the root node "a";
;;; should return SUCCESS, e.g.
;;; > (test1)
;;; SUCCESS
(define (test1)
  ;;(trace filter-kids keep-if)
  (bfs (lambda (x) (eq? x 'm)) kids1 '(a) '()))

(test1)