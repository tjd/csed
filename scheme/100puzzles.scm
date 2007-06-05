;;; 100puzzles.scm

;;; Average at-home score: 217/100 = 2.17 tiles (so 8 - 2.17 = 5.83 tiles not at home, on average)
;;; Average manhattan score: 244/25 = 9.76 tiles

(define 100puzzles
  '(#(1 3 8 5 2 7 4 6 *) #(2 5 3 4 1 6 7 8 *) #(* 2 3 1 5 8 6 7 4) #(* 2 3 1 8 4 7 5 6) #(3 5 6 1 * 2 4 7 8) #(2 3 * 4 6 8 7 1 5) #(1 2 3 7 * 8 5 6 4) #(2 3 4 1 6 8 5 7 *) #(4 1 3 7 * 2 8 6 5) #(3 4 5 1 * 2 7 8 6) #(4 1 3 2 * 6 7 5 8) #(2 5 6 1 3 8 * 7 4) #(1 2 3 6 * 8 4 7 5) #(2 5 6 1 4 3 7 8 *) #(2 3 6 1 * 4 7 5 8) #(1 2 3 7 4 5 8 6 *) #(2 6 3 1 * 8 7 5 4) #(4 1 3 2 5 6 7 8 *) #(2 6 3 1 7 8 * 4 5) #(1 2 3 4 6 8 * 7 5) #(* 2 3 1 7 6 5 4 8) #(2 5 3 8 * 7 1 4 6) #(2 3 6 1 7 5 4 8 *) #(2 5 3 1 * 8 4 6 7) #(1 2 8 3 * 4 6 5 7) #(1 2 3 7 4 8 * 6 5) #(* 2 3 7 5 6 1 4 8) #(* 8 3 2 4 1 7 6 5) #(1 2 3 5 7 6 * 4 8) #(* 2 3 1 7 5 6 8 4) #(1 3 6 4 * 2 7 5 8) #(2 3 8 1 * 6 4 7 5) #(8 3 6 2 1 7 4 5 *) #(7 1 3 5 2 6 * 4 8) #(* 1 5 4 3 2 7 8 6) #(1 2 3 4 8 5 7 6 *) #(1 3 * 4 2 8 7 6 5) #(1 2 3 4 8 5 7 6 *) #(4 1 3 2 * 5 7 8 6) #(6 3 8 2 1 4 * 7 5) #(4 1 3 2 6 8 7 5 *) #(1 6 2 5 * 3 4 7 8) #(1 3 6 5 7 2 * 4 8) #(4 1 3 2 * 8 7 6 5) #(1 2 3 7 * 8 6 4 5) #(7 8 5 3 * 1 2 4 6) #(3 4 5 8 * 2 1 7 6) #(1 2 3 4 8 5 * 7 6) #(5 6 3 2 8 7 1 4 *) #(1 3 5 8 * 2 4 7 6) #(1 3 6 4 5 2 * 7 8) #(4 1 3 2 * 6 7 5 8) #(2 3 5 1 8 7 6 4 *) #(1 8 3 7 4 2 * 5 6) #(2 7 3 1 * 6 8 5 4) #(2 4 3 1 5 6 7 8 *) #(2 3 6 7 * 4 1 8 5) #(2 3 7 5 6 8 1 4 *) #(1 2 3 8 * 7 4 6 5) #(4 1 6 7 * 2 3 8 5) #(4 1 6 7 * 8 2 3 5) #(1 8 * 4 6 2 3 7 5) #(3 5 6 4 * 1 2 7 8) #(* 4 3 7 1 8 6 2 5) #(1 3 8 4 2 5 * 7 6) #(3 8 4 2 1 6 5 7 *) #(* 2 3 1 7 6 5 4 8) #(4 5 3 2 6 8 1 7 *) #(5 6 2 4 * 3 7 8 1) #(1 3 6 4 5 2 * 7 8) #(1 3 6 7 4 2 5 8 *) #(1 2 6 4 * 3 7 8 5) #(3 6 2 1 * 5 4 7 8) #(* 1 6 4 3 2 7 5 8) #(3 6 2 1 * 8 7 4 5) #(7 1 3 2 * 6 5 4 8) #(1 6 5 4 3 2 7 8 *) #(2 5 6 1 8 4 7 3 *) #(3 1 6 2 * 4 7 5 8) #(2 3 * 1 7 4 5 8 6) #(1 2 3 4 5 6 * 7 8) #(1 3 6 4 8 5 7 2 *) #(1 3 8 4 * 5 7 2 6) #(1 3 5 8 7 2 * 4 6) #(1 2 3 6 * 4 5 7 8) #(7 1 3 2 8 4 * 5 6) #(* 3 8 2 6 1 7 4 5) #(1 2 3 4 8 5 7 6 *) #(1 3 5 7 * 6 2 4 8) #(4 3 6 2 * 1 7 5 8) #(* 3 5 1 4 8 7 6 2) #(3 6 7 1 5 2 4 8 *) #(1 2 3 4 6 8 7 5 *) #(5 3 8 2 6 7 1 4 *) #(1 7 5 4 * 2 8 3 6) #(2 7 * 1 3 5 4 8 6) #(2 1 6 3 * 8 4 7 5) #(5 3 6 7 * 1 2 4 8) #(4 1 3 7 * 5 8 2 6) #(1 4 6 5 7 3 2 8 *)))