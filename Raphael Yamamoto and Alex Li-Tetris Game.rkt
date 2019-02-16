;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Raphael Yamamoto and Alex Li-Tetris Game|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Our Tetris Game

;;recursive template:
;;  (define (recursive list func))
;;

;;World Struct
;; contains the matrix, the score, and the active tetra (the one that is falling)

;

(define-struct world (matrix active-tetra score))

;; Grid
;; a 10 by 20 grid, each block

(define WIDTH 10)
(define HEIGHT 20)
(define BLOCK-SIZE 50) ;; size of individual block in grid

(define grid (make-grid WIDTH HEIGHT BLOCK-SIZE))

;; Matrix
;; The matrix is a 2D lists of list

;;make-grid : Number Number Number -> Image
;;  given width, height, block-size makes a grid image
;;

#|(define (make-grid)
  (recur )
  )|#

;;recur: list of anything function -> list of anything
;;  applies function to each element of the list and returns a list
;;

(define (make-grid WIDTH HEIGHT BLOCK-SIZE)
  
  )

;; recurfunc : number number size-> image

#;(define (recur my-list func)
  (cond
    [(empty? my-list) empty]
    [else (cons (func (first (my-list))) (recur (rest my-list) func))])
  )


(define (plus-1 n)
  (+ n 1))
(define (do-to-every-num f l)
  (cond
    [(empty? l) empty]
    [else (cons (f (first l)) (do-to-every-num f (rest l)))] ))

;recur : start end f 

                 

;; Tetra block : Struct which contains the color of the tetra
;; the center of the tetra in which it rotates, and the rotation mode;
;; if the rotation point is at a corner of the block or if it is 

(struct tetra (color center center-corner? blocks))

(define (O-tetra "green" (posn 1 -1) true
                 (list (posn 0 0) (posn 1 0) (posn 0 -1) (posn -1 -1))))
;; the o block, which will be green, its 

(define (L-tetra "purple" (posn 1 -1) false
                 (list (posn 0 -1) (posn 1 -1) (posn 2 -1) (posn 2 0))))

(define (J-tetra "cyan" (posn 1 -1) false
                 (list (posn 0 -1) (posn 1 -1) (posn 2 -1) (posn 0 0))))

(define (I-tetra "darkblue" (posn 2 -1) true
                 (list (posn 0 0) (posn 1 0) (posn 2 0) (posn 3 0))))

(define (T-tetra "orange" (posn 1 -1) false
                 (list (posn 0 -1) (posn 1 -1) (posn 1 0) (posn 2 -1))))

(define (Z-tetra "pink" (posn 1 -1) false
                 (list (posn 0 0) (posn 1 0) (posn 1 -1) (posn 2 -1))))

(define (S-tetra "red" (posn 1 -1) false
                 (list (posn 0 -1) (posn 1 -1) (posn 1 0) (posn 2 0))))

;; recur : num num num (num -> num) -> (num -> num)
;; takes in a start, end, func -> start and end are now f(start) and f(end)
;; 




#; (define (recur index check repeat func)
  ;; index is a number, check, repeat, and index are all functions, 
  (cond
   [(not(check index))]
  ))

;; Draw Function : world -> image
;;  given worldstate, returns an image
;;

#;(define (draw w)
  (draw-tetra (world-active-tetra w) grid)
  )

;;falalalalala

#;(big-bang (world 0 T-tetra 0)
          [on-tick tick]
          [on-draw draw]
          [on-key key])