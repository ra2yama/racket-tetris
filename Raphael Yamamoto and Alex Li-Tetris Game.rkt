;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Raphael Yamamoto and Alex Li-Tetris Game|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
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

;; (define grid (make-grid WIDTH HEIGHT BLOCK-SIZE))

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

;;(define (recur my-list func)
 ;; (cond
    ;;[(empty? my-list) empty]
   ;; [else (cons (func (first (my-list))) (recur (rest my-list) func))])
  ;;)

;;recur-by-number : Number Number Lambda -> Lambda
;;  runs function for every number of thing
;;
;;(recur-by-number 10 0 (lambda (num) 10))) -> 0,1,2,3,4,5,6,7,8,9,10

(define (recur-by-number max index func)
  (cond
    [(>= index max) (func index)]
    [else (func index) (recur-by-number max (+ index 1) func)])
  )

;; Draw Function : world -> image
;;  given worldstate, returns an image
;;

(define (draw w)
  grid
  )

;;falalalalala

(big-bang 0
          [on-tick tick]
          [on-draw draw]
          [on-key key])