;;; 8puzzle.scm

(load "tools.scm")

                                 ;; tile locations
(define start-board '#(1 2 3     ;; 0 1 2
                       4 5 6     ;; 3 4 5
                       7 8 *))   ;; 6 7 8

(define (blank? s)
  (eq? s '*))

;;; check if board has all tiles in order
(define (solved? board)
  (equal? board start-board))

(define (next-boards board)
  (let ((swap (lambda (i j) (swap-tiles i j board))))
    (cond ((blank? (vector-ref board 0)) (list (swap 0 1) (swap 0 3)))
          ((blank? (vector-ref board 1)) (list (swap 1 0) (swap 1 2) (swap 1 4)))
          ((blank? (vector-ref board 2)) (list (swap 2 1) (swap 2 5)))
          ((blank? (vector-ref board 3)) (list (swap 3 0) (swap 3 4) (swap 3 6)))
          ((blank? (vector-ref board 4)) (list (swap 4 1) (swap 4 3) (swap 4 5) (swap 4 7)))
          ((blank? (vector-ref board 5)) (list (swap 5 4) (swap 5 8)))
          ((blank? (vector-ref board 6)) (list (swap 6 3) (swap 6 7)))
          ((blank? (vector-ref board 7)) (list (swap 7 4) (swap 7 6) (swap 7 8)))
          ((blank? (vector-ref board 8)) (list (swap 8 5) (swap 8 7)))
          )))
    
(define (swap-tiles i j board)
  (let ((new (vector-copy board)))
    (vector-set! new i (vector-ref board j))
    (vector-set! new j (vector-ref board i))
    new))
  