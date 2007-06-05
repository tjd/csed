;;; 8puzzle.scm

;;;
;;; basic helper functions
;;;

;;; returns a random item in v
(define (vector-random-item v)
  (vector-ref v (random (vector-length v))))

;;; returns a random item in lst
(define (list-random-item lst)
  (vector-random-item (list->vector lst)))

;;; returns a copy of vector v
(define (vector-copy v)
  (let ((n (vector-length v)))
    (do ((new (make-vector n))
         (i 0 (+ i 1)))
      ((= i n) new)
      (vector-set! new i (vector-ref v i)))))

;;;
;;; 8-puzzle functions
;;;

                                 ;; tile locations
(define start-board '#(1 2 3     ;; 0 1 2
                       4 5 6     ;; 3 4 5
                       7 8 *))   ;; 6 7 8

(define (blank? s)
  (eq? s '*))

;;; check if board has all tiles in order
(define (solved? board)
  (equal? board start-board))

;;; returns a list of all possible boards that come from sliding the blank 1 location
(define (next-boards board)
  (let ((swap (lambda (i j) (swap-tiles i j board)))
        (blank-at? (lambda (t) (blank? (vector-ref board t)))))
    (cond ((blank-at? 0) (list (swap 0 1) (swap 0 3)))
          ((blank-at? 1) (list (swap 1 0) (swap 1 2) (swap 1 4)))
          ((blank-at? 2) (list (swap 2 1) (swap 2 5)))
          ((blank-at? 3) (list (swap 3 0) (swap 3 4) (swap 3 6)))
          ((blank-at? 4) (list (swap 4 1) (swap 4 3) (swap 4 5) (swap 4 7)))
          ((blank-at? 5) (list (swap 5 4) (swap 5 8)))
          ((blank-at? 6) (list (swap 6 3) (swap 6 7)))
          ((blank-at? 7) (list (swap 7 4) (swap 7 6) (swap 7 8)))
          ((blank-at? 8) (list (swap 8 5) (swap 8 7)))
          )))

;;; returns a new board with tiles at locations i and j swapped
(define (swap-tiles i j board)
  (let ((new (vector-copy board)))
    (vector-set! new i (vector-ref board j))
    (vector-set! new j (vector-ref board i))
    new))

;;; returns the resulting board after randomly moving the blank space one spot
(define (one-random-move board)
  (list-random-item (next-boards board)))

;;; returns the new board that results after doing n random moves
(define (random-move n board)
  (if (= n 0)
      board
      (random-move (- n 1) (one-random-move board))))

;;; returns the count of the number of tiles not in their home position
(define (count-home board)
  (apply + (map (lambda (a b) (if (eq? a b) 1 0)) 
                (vector->list board) 
                (vector->list start-board))))

;;; returns true if tile is at index location loc on board; false otherwise
(define (tile-at? tile loc board)
  (eq? tile (vector-ref board loc)))

;;; returns the index location of tile t on board
(define (tile-loc t board)
  (let ((at? (lambda (i) (tile-at? t i board))))
    (cond ((at? 0) 0)
          ((at? 1) 1)
          ((at? 2) 2)
          ((at? 3) 3)
          ((at? 4) 4)
          ((at? 5) 5)
          ((at? 6) 6)
          ((at? 7) 7)
          ((at? 8) 8))))

;;; returns the row in which tile t occurs in board;
;;; first row is 0, second row is 1, third row is 2
(define (tile-row t board)
  (let ((at? (lambda (i) (tile-at? t i board))))
    (cond ((or (at? 0) (at? 1) (at? 2))
           0)
          ((or (at? 3) (at? 4) (at? 5))
           1)
          (else 
           2))))
  
;;; returns the row in which tile t occurs in board;
;;; first col is 0, second col is 1, third col is 2
(define (tile-col t board)
  (let ((at? (lambda (i) (tile-at? t i board))))
    (cond ((or (at? 0) (at? 3) (at? 6))
           0)
          ((or (at? 1) (at? 4) (at? 7))
           1)
          (else 
           2))))

;;; return the home row of tile t
(define (home-row t)
  (tile-row t start-board))

;;; return the home col of tile t
(define (home-col t)
  (tile-col t start-board))

;;; returns the manhattan distance of t from its home position
(define (manhattan t board)
  (let ((trow (tile-row t board))
        (tcol (tile-col t board))
        (hrow (home-row t))
        (hcol (home-col t)))
    (+ (abs (- trow hrow))
       (abs (- tcol hcol)))))

;;; returns the manhattan heuristic value for the given board
(define (manhattan-score board)
  (apply + (map (lambda (t) (if (blank? t) 0 (manhattan t board)))
                (vector->list board))))

;;; prints the given board on the console
(define (display-board board)
  (let ((show (lambda (i) (begin 
                            (display (vector-ref board i)) 
                            (display " "))))
        (newline (lambda () (display "\n"))))
    (show 0) (show 1) (show 2) (newline)
    (show 3) (show 4) (show 5) (newline)
    (show 6) (show 7) (show 8) (newline)
    (newline)
    ))
