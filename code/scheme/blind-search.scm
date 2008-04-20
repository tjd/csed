;;; blind-search.scm

;;; The following demonstates so-called blind search algorithms. That is, they search a state space
;;; without any concern for what the states contain, other than the goal state. Thus these algorithms
;;; enumerate the state space, i.e. they visit all the states in a particular order. 
;;;
;;; In some applications, such as pathfinding, you may want to augment the algorithms to record the 
;;; path from the start to the goal. For instance, in pathfinding algorithms you will want to keep track 
;;; of the path to the goal.

(require (lib "trace.ss"))  ;; used for debugging
(load "tools.scm")          ;; remove-if is here
(load "digraph.scm")        ;; directed graph helper functions

;;; depth first search
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
(define (dfs goal? kids-fn open closed)
    (cond ((null? open) 'FAIL)
          ((goal? (car open)) (car open))
          (else
           (let ((x (car open)))
             (dfs goal? kids-fn 
                  (append (filter-kids x kids-fn open closed) (cdr open)) ;; new open list
                  (cons x closed))))))

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
;;; should return the node matching goal?, e.g.
;;; > (test1)
;;; m
(define (test1)
  (trace dfs)
  (bfs (lambda (x) (eq? x 'm))                ;; goal function
       (lambda (x) (neighbors-of x tree1))    ;; kids function
       '(a)                                   ;; open list
       '()))                                  ;; closed list


(define (test2)
  (trace bfs)
  (bfs (lambda (x) (eq? x 'm))                ;; goal function
       (lambda (x) (neighbors-of x tree1))    ;; kids function
       '(a)                                   ;; open list
       '()))                                  ;; closed list
