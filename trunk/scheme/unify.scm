;; The following is based on the LISP unification code from the 5th
;; edition of Luger's Artificial Intelligence textbook.
;; 


;; Unifies the two given patterns, returning an association list of
;; pairs indicating variable bindings. If the patterns match with no
;; bindings (e.g. they are the same pattern), then '() is returned. If
;; the patterns don't match, #f is returned.
(define (unify pat1 pat2 sub)
  (cond ((equal? sub #f) #f)
	((var? pat1) (match-var pat1 pat2 sub))
	((var? pat2) (match-var pat2 pat1 sub))
	((constant? pat1)
	 (if (equal? pat1 pat2)
	     sub
	     #f))
	((constant? pat2) #f)
	(else (unify (cdr pat1) (cdr pat2)
		     (unify (car pat1) (car pat2) sub)))))

(define (match-var var pat sub)
  (if (equal? var pat) 
      sub
      (let ((binding (get-binding var sub)))
	(cond ((not (null? binding)) 
	       (unify (get-binding-value binding) pat sub))
	      ((occurs? var pat) #f)
	      (else (add-substitution var pat sub))))))

(define (occurs? var pat)
  (cond ((equal? var pat) #t)
	((or (var? pat) (constant? pat)) #f)
	(else (or (occurs? var (car pat))
		  (occurs? var (cdr pat))))))

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (constant? item)
  (or (null? item) (atom? item)))
	
;; vars are atoms beginning with ?
(define (var? a)
  (if (symbol? a)
      (let ((s (symbol->string a)))
	(and (< 1 (string-length s))
	     (eq? #\? (string-ref s 0))))
      #f))

;; Adds (key . val) to the given association, returning the new alist.
(define (acons key val alist)
  (cons (cons key val) alist))

;; Common LISP's assoc returns nil when there's no match,
;; so that is simulated here.
;; http://www.lispworks.com/documentation/HyperSpec/Body/f_assocc.htm
(define (get-binding var sub)
  (let ((result (assoc var sub)))
    (if result
	result
	'())))

(define (get-binding-value binding)
  (cdr binding))

(define (add-substitution var pat sub)
  (acons var pat sub))
		     
  