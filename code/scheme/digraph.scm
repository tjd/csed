;;; digraph.scm
;;;
;;; Helper functions for directed graphs stored as alists.

;; return the children of a given node
(define (neighbors-of x graph)
  (cadr (assoc x graph)))

;; return the parent of the node x in graph
(define (parent-of x graph)
  (cond ((null? graph) '())
        ((memq x (cadr (car graph))) (caar graph))
        (else (parent-of x (cdr graph)))))

;;; returns the number of edges emanating from x
(define (out-degree x graph)
  (length (neighbors-of x graph)))

;;; returns the number of nodes in the graph
(define (num-nodes graph)
  (length graph))

;;; returns the number of edges in the graph
(define (num-edges graph)
  (apply + (map (lambda (p) (length (cadr p))) graph)))

;;; a few sample graphs
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
    (j (h i))))