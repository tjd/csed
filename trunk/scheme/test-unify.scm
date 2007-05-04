(load "amb.scm")
(load "tools.scm")
(load "unify.scm")

(define (test1)
  (let ((x (element-of '((bit 0) (bit 1)))))
    (unify '(bit ?x) x '())))

(define (test2)
  (let ((x (element-of '((bit 0) (bit 1)))))
    (bag-of (assert (unify '(bit ?x) x '())))))

;;;
;;; > (unify '(mother ?x bob) '(mother jane ?y))
;;; ((?y . bob) (?x . jane))
;;;
;;; > (unify '(mother ?y ?z) '(mother jane ?y))
;;; ((?z . jane) (?y . jane))
;;;

;;;
;;; blocks world example
;;;
;;;   a 
;;;   b c
(define blocks-world-state1
  '((on a b)
    (on b table)
    (on c table)
    (clear a)
    (clear c)))

;; returns all the items n lst that unify with query
(define (all-matches query lst)
  (keep-if (lambda (x) (unify query x)) lst))

;;;
;;; Find all the blocks on the table:
;;; > (all-matches '(on ?x table) blocks-world-state1)
;;; (on b table) (on c table))
;;;
;;; Find all the clear blocks:
;;; > (all-matches '(clear ?x) blocks-world-state1)
;;; ((clear a) (clear c))
;;;
;;; What is supporting a?
;;; > (all-matches '(on a ?what) blocks-world-state1)
;;; ((on a b))


    