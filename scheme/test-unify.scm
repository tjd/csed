(load "amb.scm")
(load "unify.scm")

(define (test1)
  (let ((x (element-of '((bit 0) (bit 1)))))
    (unify '(bit ?x) x '())))

(define (test2)
  (let ((x (element-of '((bit 0) (bit 1)))))
    (bag-of (assert (unify '(bit ?x) x '())))))