;;; bfs.scm
(require (lib "trace.ss"))  ;; used for debugging
(load "tools.scm")          ;; remove-if is here

;;; breadth first search
;;;
;;; Searches in a given graph in a bread-first manner. Returns a node satisfying the goal function,
;;; and FAIL otherwise.
;;;
;;; goal-fn: a function that takes a node as input, and returns true if the node
;;;          satisfies the goal, and false otherwise
;;; kids-fn: a function that takes a node as input, and returns its children
;;; open: a starting list of nodes; normally initialized to the (single) starting
;;;       state
;;; closed: the list of nodes that have been visited; this makes sure that the search doesn't
;;;         stuck in a loop in the graph
(define (bfs goal-fn kids-fn open closed)
    (cond ((null? open) 'FAIL)
          ((goal-fn (car open)) (car open))
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

;;;
;;; tree1
;;;
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
(define (kids x graph)
  (cadr (assoc x graph)))

;; return the parent of the node x in graph
(define (parent-of x graph)
  (cond ((null? graph) '())
        ((memq x (cadr (car graph))) (caar graph))
        (else (parent-of x (cdr graph)))))

;;; search for node "m" starting at the root node "a";
;;; should return the node matching goal-fn, e.g.
;;; > (test1)
;;; m
(define (test1)
  ;;(trace bfs)
  (bfs (lambda (x) (eq? x 'm)) (lambda (x) (kids x tree1)) '(a) '()))

;;;
;;; graph1
;;;

(define graph1
  '((a (b f))
    (b (a c f g h))
    (c (b d))
    (d (c e))
    (e (d h))
    (f (a b g i))
    (g (b h i))
    (h (b e g j))
    (i (f g j))
    (j (h i)))