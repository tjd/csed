;;; 8puzzle.scm
                                  ;; tile locations
(define start-puzzle '#(1 2 3     ;; 0 1 2
                        4 5 6     ;; 3 4 5
                        7 8 *))   ;; 6 7 8

(define (blank? s)
  (eq? s '*))

;;; check if board has all tiles in order
(define (solved? board)
  (equal? board start-puzzle))

(define (next-boards board)
  (cond ((blank? (vector-ref board 0)) '(swap 0 1 0 3))
        ((blank? (vector-ref board 1)) '(swap 1 0 1 2 1 4))
        ((blank? (vector-ref board 2)) '(swap 2 1 2 5))
        ((blank? (vector-ref board 3)) '(swap 3 0 3 4 3 6))
        ((blank? (vector-ref board 4)) '(swap 4 1 4 3 4 5 4 7))
        ((blank? (vector-ref board 5)) '(swap 5 4 5 8))
        ((blank? (vector-ref board 6)) '(swap 6 3 6 7))
        ((blank? (vector-ref board 7)) '(swap 7 4 7 6 7 8))
        ((blank? (vector-ref board 8)) '(swap 8 5 8 7))
        ))

(defiune (swap-tiles i j board)
  
  