
(define var?
  (lambda (a)
    (let ((s (symbol->string a)))
      (and (< 1 (string-length s))
	   (eq? #\? (string-ref s 0))))))

(define make-var
  (lambda (a)
    (string->symbol (string-append "?" (symbol->string a)))))
