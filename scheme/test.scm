(require (lib "defmacro.ss"))

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


(define var?
  (lambda (a)
    (let ((s (symbol->string a)))
      (and (< 1 (string-length s))
	   (eq? #\? (string-ref s 0))))))

(define make-var
  (lambda (a)
    (string->symbol (string-append "?" (symbol->string a)))))
