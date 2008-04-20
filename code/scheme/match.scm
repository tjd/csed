;; The following is based on the LISP matching code from the 5th
;; edition of Luger's Artificial Intelligence textbook.
;; 
;; Code using (amb) is my addition.

(load "amb.scm")

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (var? a)
    (eq? a '?))

;; Atoms are equal of they are the same, or if one or both
;; is a variable. In Luger's code, '() is considered an atom.
(define (match-atom? pat1 pat2)
  (or (equal? pat1 pat2)
      (var? pat1)
      (var? pat2)
))

;; Two patterns match if they are atoms that match, or, if they
;; are lists, their cars and cdrs match.
(define (match? pat1 pat2)
  (cond ((or (null? pat1) (null? pat2) (atom? pat1) (atom? pat2))
	 (match-atom? pat1 pat2))
	(else (and (match? (car pat1) (car pat2))
		   (match? (cdr pat1) (cdr pat2))))))

;; Returns list of all items in database that match pat.
(define (get-all-matches pat database)
  (cond ((null? database) '())
	((match pat (car database))
	 (cons (car database) 
	       (get-all-matches pat (cdr database))))
	(else (get-all-matches pat (cdr database)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following two functions using the non-deterministic
;; amb operator to do the matching --- the code is much 
;; simpler than the get-all-matches function!
(define (get-match-nd pat database)
  (let ((m (element-of database)))
    (assert (match? pat m))
    m))

(define (get-all-matches-nd pat database)
  (bag-of (get-match-nd pat database)))

(define movie-database
  '(
    ((william macy) actor 1950)
    ((marisa tomei) actress 1964)
    ((drew sidora) actress 1985)
    ((john hurt) actor 1940)
    ((steven speilberg) director 1946)
    ((george lucas) director 1944)
    ((william hurt) actor 1950)
    ((matt damon) actor 1970)
    )
)