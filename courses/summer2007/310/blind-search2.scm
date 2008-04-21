;;;
;;; blind-search2.scm
;;;

;;; The following demonstates so-called blind search algorithms. That is, they 
;;; search a state space without any concern for what the states contain, other 
;;; than the goal state. Thus these algorithms enumerate the state space, i.e. 
;;; they visit all the states in a particular order.
;;;
;;; An interesting detail of this implementation is that it uses closures to
;;; simulate object-like implementations of stacks and queues. Little regard has 
;;; been given to performance, so if you plan to use these in performance-critical 
;;; applications you can and should improve upon their implementation.

;;; > (define s (make-stack))
;;; > s
;;; #<procedure>
;;; > (s 'empty?)
;;; #t
;;; > (s 'size)
;;; 0
;;; > (s '(push x))
;;; > (s 'body)
;;; (x)
;;; > (s 'size)
;;; 1
;;; > (s 'peek)
;;; x
;;; > (s 'empty?)
;;; #f
;;; > (define s (make-stack))
;;; > (s 'empty?)
;;; #t
;;; > (s '(member 5))
;;; #f
;;; > (s '(push 5))
;;; > (s 'body)
;;; (5)
;;; > (s '(member 5))
;;; (5)
;;; > (s '(member five))
;;; #f
(define (make-stack)
  (let ((stack '()))               
    (lambda (fname)
      (if (list? fname)
          (let* ((f (car fname))
                 (p (cadr fname))
                 (is? (lambda (x) (eq? f x))))
            (cond ((is? 'push) (set! stack (cons p stack)))
                  ((is? 'push-all) (set! stack (append (reverse p) stack)))
                  ((is? 'member) (member p stack))
                  (else (begin (display "Stack error: '")
                               (display fname)
                               (display "' is unknown")))))                             
          ;;; else
          (let ((is? (lambda (x) (eq? fname x))))
            (cond ((is? 'empty?) (null? stack))
                  ((is? 'size) (length stack))
                  ((is? 'body) stack)
                  ((is? 'peek) (car stack))
                  ((is? 'pop) (let ((top (car stack)))
                                (set! stack (cdr stack))
                                top))          
                  (else (begin (display "Stack error: '")
                               (display fname)
                               (display "' is unknown")))))))))

;;; > (define q (make-queue))
;;; > (q '(push 4))
;;; > (q '(push 5))
;;; > (q '(push 6))
;;; > (q 'body)
;;; (6 5 4)
;;; > (q 'pop)
;;; 4
;;; > (q 'pop)
;;; 5
;;; > (q 'pop)
;;; 6
;;; > (q 'length)
;;; Queue error: 'length' is unknown
;;; > (q 'size)
;;; 0
;;; > (q '(push over))
;;; > (q '(member over))
;;; (over)
;;; > (q '(member all))
;;; #f
(define (make-queue)
  (let ((queue '()))               
    (lambda (fname)
      (if (list? fname)
          (let* ((f (car fname))
                 (p (cadr fname))
                 (is? (lambda (x) (eq? f x))))
            (cond ((is? 'push) (set! queue (cons p queue)))
                  ((is? 'push-all) (set! queue (append (reverse p) queue)))
                  ((is? 'member) (member p queue))
                  (else (begin (display "Queue error: '")
                               (display fname)
                               (display "' is unknown!")))))                             
          ;;; else 
          (let ((is? (lambda (x) (eq? fname x))))
            (cond ((is? 'empty?) (null? queue))
                  ((is? 'size) (length queue))
                  ((is? 'body) queue)
                  ((is? 'peek) (car (reverse queue)))
                  ((is? 'pop) (let* ((rev (reverse queue))
                                     (end (car rev)))
                                (set! queue (reverse (cdr rev)))
                                end))
                  (else (begin (display "Queue error: '")
                               (display fname)
                               (display "' is unknown")))))))))

;;;
;;; basic blind search functions
;;;

(load "tools.scm")

;;; Generic search algorithm; open must be implemented in the style of
;;; make-stack and make-queue above.
(define (search open closed goal? children-of)
  (if (open 'empty?)
      'FAIL
      (let ((x (open 'pop)))
        (display x) (display " ")
        (if (goal? x)
            'SUCCESS
            (let* ((new-closed (cons x closed))
                   (all-kids (children-of x))
                   (kids (remove-if (lambda (s) (or (member s new-closed)
                                                    (open (list 'member s))))
                                    all-kids)))
              (open (list 'push-all kids))   ;;; add the new nodes to check
              (search open
                      new-closed
                      goal?
                      children-of))))))

;;; Depth first search
;;;         goal? - a function that takes a single state as input, and returns
;;;                 true if it's goal state, and false otherwise
;;;   children-of - a function that takes a single state as input, and returns
;;;                 the list of children of that state; if the state is childless,
;;;                 the empty list is returned
(define (depth-first start goal? children-of)              
  (let ((stack (make-stack)))
    (stack (list 'push start))
    (search stack '() goal? children-of)))

;;; Breadth first search
;;;         goal? - a function that takes a single state as input, and returns
;;;                 true if it's goal state, and false otherwise
;;;   children-of - a function that takes a single state as input, and returns
;;;                 the list of children of that state; if the state is childless,
;;;                 the empty list is returned
(define (breadth-first start goal? children-of)              
  (let ((queue (make-queue)))
    (queue (list 'push start))
    (search queue '() goal? children-of)))

;;; sample graph from page 135 of Luger
(define graph1
  '((a (b c d))
    (b (e f))
    (c (g h))
    (d (i j))
    (e (k l))
    (f (l m))
    (g (n))
    (h (o p))
    (i (p q))
    (j (r))
    (k (s))
    (l (t))
    ))  

(define graph2
  '((a (b c d))
    (c (e f g))
    (d (h i))
    (f (j k))))

;;; makes a children-of function when given a graph in the form
;;; of an association list
(define (make-children-of graph)
  (lambda (s)
    (let ((p (assoc s graph)))
      (if (not p)
          '()
          (cadr p)))))

(breadth-first 'a (lambda (s) (eq? s 'k)) (make-children-of graph2))
;(depth-first 'a (lambda (s) (eq? s 'p)) (make-children-of graph1))