(load "tools.scm")
(load "amb.scm")

(define (double-list? x)
  (and (list? x) (= 2 (length x))))

(define (triple-list? x)
  (and (list? x) (= 3 (length x))))

(define (not-list? x)
  (and (double-list? x) (eq? (car x) 'not)))

(define (and-list? x)
  (and (triple-list? x) (eq? (cadr x) 'and)))

(define (or-list? x)
  (and (triple-list? x) (eq? (cadr x) 'or)))

(define (imp-list? x)
  (and (triple-list? x) (eq? (cadr x) 'imp)))

(define (prop-var? x)
  (atom? x))

(define (truth-value? x)
  (member x '(true false)))

(define (bool x)
  (if (eq? x 'true) #t #f))

(define (truth-value b)
  (if b 'true 'false))

;; Tests if x is a well-formed in-fix propositional formula. 
;; e.g.
;; > (prop-wff? '((not P) and (not (not Q))))
;; #t
;; > (prop-wff? '((not (or A B)) and (not (not Q))))
;; #f
(define (prop-wff? x)
  (cond ((prop-var? x) #t)
        ((truth-value? x) #t)
        ((not-list? x) (prop-wff? (cadr x)))
        ((and-list? x) (and (prop-wff? (car x)) (prop-wff? (caddr x))))
        ((or-list? x) (and (prop-wff? (car x)) (prop-wff? (caddr x))))
        ((imp-list? x) (and (prop-wff? (car x)) (prop-wff? (caddr x))))
        (else #f)))

;; returns true iff x is a wff with no propositional variables, just
;; truth values
;; e.g.
;; > (ground-wff? '(not (false or (true and (not false)))))
;; #t
;; > (ground-wff? '(not ((false or A) and (false imp T))))
;; #f
(define (ground-wff? x)
  (cond ((truth-value? x) #t)
        ((not-list? x) (ground-wff? (cadr x)))
        ((and-list? x) (and (ground-wff? (car x)) (ground-wff? (caddr x))))
        ((or-list? x) (and (ground-wff? (car x)) (ground-wff? (caddr x))))
        ((imp-list? x) (and (ground-wff? (car x)) (ground-wff? (caddr x))))
        (else #f)))

(define (ground-eval-aux x)
  (cond ((not (ground-wff? x)) 'error)
        ((truth-value? x) (bool x))
        ((not-list? x) (not (ground-eval-aux (cadr x))))
        ((and-list? x) (and (ground-eval-aux (car x)) (ground-eval-aux (caddr x))))
        ((or-list? x) (or (ground-eval-aux (car x)) (ground-eval-aux (caddr x))))
        ((imp-list? x) (or (not (ground-eval-aux (car x))) (ground-eval-aux (caddr x))))
        (else 'error)))

;; evaluates a ground wff
(define (ground-eval x)
  (truth-value (ground-eval-aux x)))

;; non-deterministically returns an association list of truth-values for x
;; e.g.
;; > (bag-of (assign-vars '(x or y) '()))
;; (((y true) (x true)) ((y false) (x true)) ((y true) (x false)) ((y false) (x false)))
;; > (bag-of (assign-vars '((z and (not y)) or (y and (not z))) '()))
;; (((y true) (z true)) ((y false) (z true)) ((y true) (z false)) ((y false) (z false)))
(define (assign-vars x env)
  (cond ((truth-value? x) env)
        ((prop-var? x) (if (assq x env)
                           env
                           (cons (list x (element-of '(true false))) env))) ;; nd-call
        ((not-list? x) (assign-vars (cadr x) env))
        ((or (and-list? x) (or-list? x) (imp-list? x)) 
         (assign-vars (caddr x)
                      (assign-vars (car x) env)))))

(define (eval-aux x env)
  (cond ((truth-value? x) (bool x))
        ((prop-var? x) (bool (cadr (assq x env))))
        ((not-list? x) (not (eval-aux (cadr x) env)))
        ((and-list? x) (and (eval-aux (car x) env)
                            (eval-aux (caddr x) env)))
        ((or-list? x) (or (eval-aux (car x) env)
                          (eval-aux (caddr x) env)))
        ((imp-list? x) (or (not (eval-aux (car x))) 
                           (eval-aux (caddr x))))
        (else 'error)))

;; evaluates propositional expression x using the given assignments of values to variables
(define (eval x env)
  (truth-value (eval-aux x env)))

;; non-deterministically generates assignments of truth-values to x's variables
;; that make x true; uses a truth table, so the performance is exponential in
;; the number of atoms
(define (satisfy x)
  (let* ((env (assign-vars x '()))
         (result (eval x env)))
    (assert (eq? 'true result))
    env))
    
(define (contradiction x)
  (null? (bag-of (satisfy x))))

(define (tautology x)
  (contradiction (list 'not x)))

;; returns #t iff x and y are logically equivalent
(define (logically-equiv x y)
  (tautology (list 'and 
                   (list x 'imp y)
                   (list y 'imp x))))
