(load "unify.scm")

(define facts
  '((likes bob candy)
    (likes mary candy)
    (likes bob mary)
    (likes dan candy)
    (likes dan glass)
    (likes bob glass)
    (shiny mirror)
    (shiny glass)
    (shiny desk)
    (father-of bill jess)
    (father-of bill cat)
    (father-of dan bill)
    (father-of dan bob)
    (father-of dan mary)
    ))

;;; return a list containing just those elements that satisfy the test function
(define (keep-if test-fn lst)
  (cond ((null? lst) '())
        ((test-fn (car lst)) (cons (car lst) (keep-if test-fn (cdr lst))))
        (else (keep-if test-fn (cdr lst)))))

;;; return a list with all items satisfying test-fn removed
(define (remove-if test-fn lst)
  (keep-if (lambda (x) (not (test-fn x))) lst))

;;; return all the facts that match e
(define (get-all e)
  (keep-if (lambda (x) (unify e x)) facts))

(define (get-all2 e)
  (let* ((u (map (lambda (x) (unify e x)) facts))     ;;; replace all facts with unifications
         (unof (remove-if (lambda (x) (eq? x #f)) u)) ;;; remove everything that didn't unify
         (results (map (lambda (lst) (map cdr lst)) unof))
         )
    results))