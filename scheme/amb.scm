;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; amb, from http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-16.html#node_idx_470
(require (lib "defmacro.ss"))
;(load "tools.scm")

(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
      (lambda ()
        (error "amb tree exhausted")))))

(initialize-amb-fail)

(define-macro amb
  (lambda alts...
    `(let ((+prev-amb-fail amb-fail))
       (call/cc
        (lambda (+sk)

          ,@(map (lambda (alt)
                   `(call/cc
                     (lambda (+fk)
                       (set! amb-fail
                         (lambda ()
                           (set! amb-fail +prev-amb-fail)
                           (+fk 'fail)))
                       (+sk ,alt))))
                 alts...)

          (+prev-amb-fail))))))

(define-macro bag-of
  (lambda (e)
    `(let ((+prev-amb-fail amb-fail)
           (+results '()))
       (if (call/cc
            (lambda (+k)
              (set! amb-fail (lambda () (+k #f)))
              (let ((+v ,e))
                (set! +results (cons +v +results))
                (+k #t))))
           (amb-fail))
       (set! amb-fail +prev-amb-fail)
       (reverse! +results))))

;; Non-deterministically returns a number from lo to hi, inclusive.
(define number-between
  (lambda (lo hi)
    (let loop ((i lo))
      (if (> i hi) (amb)
          (amb i (loop (+ i 1)))))))

;; Non-deterministically returns an element of lst.
(define element-of
  (lambda (lst)
    (if (= 1 (length lst))
        (car lst)
        (amb (car lst) (element-of (cdr lst))))))

(define assert
  (lambda (pred)
    (if (not pred) (amb))))

(define assert2
  (lambda (pred)
    (if (not pred) 
        (amb)
        pred)))

