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

(define sum
  (lambda (n)
    (if (<= n 0)
	0
	(+ n (sum (sub1 n))))))

(define divides
  (lambda (a b)
    (= 0 (modulo b a))))

(define get_lowest_divisor
  (lambda (n div)
    (if (divides div n)
	div
	(get_lowest_divisor n (add1 div)))))

(define divisors_aux
  (lambda (n div)
    (cond 
      ((< n div) '())
      ((= n div) (list div))
      (else
       (let ((ld (get_lowest_divisor n div)))
         (cons ld (divisors_aux n (add1 ld)))
         )))))

(define divisors (lambda (n) (divisors_aux n 1)))
(define d (lambda (n) (length (divisors n))))
(define prime? (lambda (n) (= 2 (d n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define is-sorted?
  (lambda (lst)
    (cond ((null? (cdr lst)) #t)
          (else (and (<= (car lst) (cadr lst)) (is-sorted? (cdr lst)))))))

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

(define all?
  (lambda (lst test?)
    (if (null? lst) #t
        (and (test? (car lst)) (all? (cdr lst) test?)))))

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
    (cond ((null? lst) #f)
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

(define range (lambda (n) (range_aux n n)))
                       
(define range_aux
  (lambda (i n)
    (if (< i 1)
        '()
        (cons (- n i) (range_aux (sub1 i) n)))))


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
                  (r (random (add1 (length rest))))
                  )
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
    