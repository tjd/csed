;;;
;;; tools.scm
;;;

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define singleton? 
  (lambda (lst) 
    (and (list? lst) (= 1 (length lst)))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define all-equal
  (lambda (lst)
    (cond ((null? lst) #t)
          ((singleton? lst) #t)
          (else
           (and (eq? (car lst) (cadr lst)) (all-equal (cdr lst)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define is-sorted?
  (lambda (lst)
    (cond 
      ((null? (cdr lst)) #t)
      (else 
       (and (<= (car lst) (cadr lst)) (is-sorted? (cdr lst)))))))

;;; alternatively:
;(define (is-sorted? lst)
;  (apply <= lst))

(define merge 
  (lambda (A B)
    (cond ((null? A) B)
          ((null? B) A)
          ((< (car A) (car B)) (cons (car A) (merge (cdr A) B)))
          (else (cons (car B) (merge A (cdr B)))))))

(define left
  (lambda (lst n)
    (cond ((= n 1) (list (car lst)))
          (else (cons (car lst) (left (cdr lst) (sub1 n)))))))

(define right
  (lambda (lst n)
    (reverse (left (reverse lst) n))))

(define split-aux
  (lambda (A B)
    (let* ((lenA (length A))
           (lenB (length B))
           (diff (- lenA lenB)))
      (if (<= diff 1)
          (list A B)
          (split-aux (cdr A) (cons (car A) B))))))

(define split
  (lambda (lst)
    (let* ((LR (split-aux lst '()))
           (1st (car LR))
           (2nd (cadr LR)))
      (list (reverse 2nd) 1st))))
           

(define mergesort
  (lambda (lst)
    (cond ((null? lst) '())
          ((null? (cdr lst)) lst)
          (else (let* ((sp (split lst))
                       (L (mergesort (car sp)))
                       (R (mergesort (cadr sp))))
                  (merge L R))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define all?
;;   (lambda (lst test?)
;;     (if (null? lst) #t
;;         (and (test? (car lst)) (all? (cdr lst) test?)))))

;; returns true if all the elements on lst make test? return true
(define all?
  (lambda (test? lst)
    (if (null? lst) #t
        (and (test? (car lst)) (all? test? (cdr lst))))))

(define (some? test? lst)
  (if (null? lst) #t
      (or (test? (car lst)) (some? test? (cdr lst)))))
  

;; returns a list with n copies of sexp
(define rep
  (lambda (sexp n)
    (if (<= n 0)
        '()
        (cons sexp (rep sexp (sub1 n))))))

(define count?
  (lambda (lst test?)
    (cond ((null? lst) 0)
          ((test? (car lst)) (add1 (count? (cdr lst) test?)))
          (else (count? (cdr lst) test?)))))

(define no-dups?
  (lambda (lst p)
    (cond ((null? p) #t)
          ((= 1 (count? lst (lambda (n) (= n (car p)))))
           (no-dups? lst (cdr p)))
          (else #f))))

(define member?
  (lambda (x lst)
    (cond 
      ((null? lst) #f)
      ((eq? x (car lst)) #t)
      (else (member? x (cdr lst))))))

(define rem1st
  (lambda (atm lst)
    (cond ((null? lst) '())
          ((eq? atm (car lst)) (cdr lst))
          (else (cons (car lst) (rem1st atm (cdr lst)))))))

;; remove duplicates
(define setify
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((head (car lst))
              (tail (cdr lst)))
          (if (member? head tail)
              (setify tail)
              (cons head (setify tail)))))))
               
;; returns a list of the numbers from n-1 to 0
(define nums
  (lambda (n)
    (if (<= n 0)
	'()
	(let ((n1 (sub1 n)))
	  (cons n1 (nums n1))))))

(define range (lambda (n) (range-aux n n)))
                       
(define range-aux
  (lambda (i n)
    (if (< i 1)
        '()
        (cons (- n i) (range-aux (sub1 i) n)))))

(define gen-nums 
  (lambda (n)
    (if (= n 1)
        (list (list 1 1))
        (cons (list n (d n)) (gen-nums (sub1 n))))))
 
(define twist
  (lambda (two)
    (list (cadr two) (car two))))

(define twist-all
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (twist (car lst)) (twist-all (cdr lst))))))

;; inserts a at position i in lst
(define insert
  (lambda (a i lst)
    (cond ((= i 0) (cons a lst))
          (else (cons (car lst) (insert a (sub1 i) (cdr lst)))))))

;; randomly shuffles lst
(define shuffle
  (lambda (lst)
    (cond ((null? lst) '())
          ((null? (cdr lst)) lst)
          (else
           (let* ((hd (car lst))
                  (rest (shuffle (cdr lst)))
                  (r (random (add1 (length rest)))))
             (insert hd r rest))))))

;;; swaps location i of vec with another randomly chosen location in vec
(define vec-swap-rand!
  (lambda (vec i)
    (let* ((n (vector-length vec))
           (r (random n))
           (val (vector-ref vec r)))
      (vector-set! vec r (vector-ref vec i))
      (vector-set! vec i val))))

;;; shuffles the given vector in-place
(define vec-shuffle!
  (lambda (vec)
    (for-each (lambda (i)
                (vec-swap-rand! vec i))
              (range (vector-length vec)))))

(define vec-all-aux?
  (lambda (vec f? start)
    (if (= start (sub1 (vector-length vec)))
        (f? (vector-ref vec start))
        (and (f? (vector-ref vec start))
             (vec-all-aux? vec f? (add1 start))))))

(define vec-all?
  (lambda (vec f?)
    (vec-all-aux? vec f? 0)))

;; returns a list (!) of the elements in vec from start to end-1
(define vec-slice
  (lambda (vec start end)
    (do ((result (make-vector (- end start)))
         (i start (add1 i)))
      ((>= i end) result)
      (vector-set! result (- i start) (vector-ref vec i)))))

;; if seq is a vector, it is converted to a list; otherwise, it is returned as-is
(define listify
  (lambda (seq)
    (if (vector? seq)
        (vector->list seq)
        seq)))

;; if seq is a list, it is converted to a vector; otherwise, it is returned as-is
(define vectorify
  (lambda (seq)
    (if (list? seq)
        (list->vector seq)
        seq)))

;; returns the item at location n in lst (0-based indexing)
(define nth
  (lambda (n lst)
    (if (= n 0)
        (car lst)
        (nth (sub1 n) (cdr lst)))))

;; mat is a list of lists
(define mat
  (lambda (r c lst)
    (nth c (nth r lst))))

;; convert pre-fix to infix
(define (prefix->infix expr)
  (cond ((null? expr) '())
        ((atom? expr) expr)
        (else (let ((f (car expr))
                    (s (car (cdr expr)))
                    (t (car (cdr (cdr expr)))))
                (list (prefix->infix s) f (prefix->infix t))))))

(define (pair-with-all x lst)
  (map (lambda (a) (list x a)) lst))

;; return the Cartesian product of the pairs of A and B
(define (prod A B)
  (if (null? A) 
      '()
      (append (pair-with-all (car A) B) 
              (prod (cdr A) B))))

;; return the flattened version of lst
(define (flatten lst)
  (cond ((null? lst) '())
        ((atom? (car lst)) (cons (car lst) (flatten (cdr lst))))
        (else (append (flatten (car lst)) (flatten (cdr lst))))))

;; return a list of all subsets of lst
(define (all-subsets lst)
  (if (null? lst)
      '(())
      (let ((sub (all-subsets (cdr lst))))
        (append sub
                (map (lambda (s) (cons (car lst) s))
                     sub)))))

;; returns the sum of the elements in lst
;(define (sum-list lst)
;  (if (null? lst)
;      0
;      (+ (car lst) (sum-list (cdr lst)))))

;; returns the dot product of the elements in A and B
(define (dot-prod A B)
  (sum-list (map (lambda (a b) (* a b)) A B)))

;;;
;;; see http://halogen.note.amherst.edu/~jdtang/scheme_in_48/tutorial/stdlib.html
;;; for examples of Scheme folds
;;;

;; right-associative fold operator
(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

;; left-associative fold operator
(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

;; more traditional Scheme names for folds
(define fold foldl)
(define reduce fold)

(define (sum . lst)
  (fold + 0 lst))

(define (product . lst)
  (fold * 1 lst))

;; return lst with first occurrence of item removed
(define (removeq-first item lst)
  (cond ((null? lst) '())
        ((eq? item (car lst)) (cdr lst))
        (else (cons (car lst) (removeq-first item (cdr lst))))))

;;; return a list containing just those elements that satisfy the test function
(define (keep-if test-fn lst)
  (cond ((null? lst) '())
        ((test-fn (car lst)) (cons (car lst) (keep-if test-fn (cdr lst))))
        (else (keep-if test-fn (cdr lst)))))

(define (remove-if test-fn lst)
  (keep-if (lambda (x) (not (test-fn x))) lst))

;;;
;;; returns a Gaussian probability density function (pdf) with the given parameters
;;;
(define pi 3.1415926)

(define (make-gaussian-pdf mean variance)
  (lambda (x)
    (* (/ 1 (sqrt (* variance 2 pi)))
       (exp (/ (* (- x mean) (- x mean))
               (* -2 variance))))))

