;;; bfs.scm
(require (lib "trace.ss"))  ;; used for debugging
(load "tools.scm")          ;; remove-if is here
(load "digraph.scm")        ;; directed graph helper functions

;;; breadth first search
;;;
;;; Searches in a given graph in a bread-first manner. Returns a node satisfying the goal function,
;;; and FAIL otherwise.
;;;
;;;   goal?: a function that takes a node as input, and returns true if the node
;;;          satisfies the goal, and false otherwise
;;; kids-fn: a function that takes a node as input, and returns its children
;;; open: a starting list of nodes; normally initialized to the (single) starting
;;;       state
;;; closed: the list of nodes that have been visited; this makes sure that the search doesn't
;;;         stuck in a loop in the graph
(define (bfs goal? kids-fn open closed)
    (cond ((null? open) 'FAIL)
          ((goal? (car open)) (car open))
          (else
           (let ((x (car open)))
             (bfs goal? kids-fn 
                  (append (cdr open) (filter-kids x kids-fn open closed)) ;; new open list
                  (cons x closed))))))

(define (filter-kids x kids-fn open closed)
  (let ((kids (kids-fn x)))
    (if (null? kids)
        kids
        (remove-if (lambda (a) (or (member a open) (member a closed))) kids))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sample usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; search for node "m" starting at the root node "a";
;;; should return the node matching goal-fn, e.g.
;;; > (test1)
;;; m
(define (test1)
  (trace bfs)
  (bfs (lambda (x) (eq? x 'm))                ;; goal function
       (lambda (x) (neighbors-of x tree1))    ;; kids function
       '(a)                                   ;; open list
       '()))                                  ;; closed list
