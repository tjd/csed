(load "amb.scm")

(define colors
  (lambda ()
    (amb 'red 'green 'blue)))

(define graph1 
  '((border sussex kent)
    (border sussex surrey)
    (border surrey kent)
    (border hampshire sussex)
    (border hampshire surrey)
    (border hampshire berkshire)
    (border berkshire surrey)
    (border wiltshire hampshire)
    (border wiltshire berkshire))
  )

(define assign-color
  (lambda (region color)
    (list region color)))

(define get-regions-aux
  (lambda (graph)
    (if (null? graph)
        '()
        (let* ((head (car graph))
               (tail (cdr graph))
               (reg1 (cadr head))
               (reg2 (caddr head)))
          (cons reg1 (cons reg2 (get-regions-aux tail)))))))

(define get-regions
  (lambda (graph)
    (setify (get-regions-aux graph))))

;(define color-map 
;  (lambda (graph)
    
    