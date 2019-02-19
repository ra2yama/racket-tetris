;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Raphael Yamamoto and Alex Li-Tetris Game|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Our Tetris Game

;;recursive template:
;;  (define (recursive list func))
;;

;;MS. JAFARI NOTES:
;;Grade: A+++++++++++++++++++
;;nice job guys your use of begin was amazign

;;World Struct
;; contains the matrix, the score, and the active tetra (the one that is falling)

(require 2htdp/universe)
(require 2htdp/image)

(define-struct world (matrix active-tetra score))

;; Grid
;; a 10 by 20 grid, each block

(define WIDTH 10)
(define HEIGHT 20)

(define BLOCK-SIZE 40) ;; size of individual block in grid

(define TOTAL-WIDTH (* WIDTH BLOCK-SIZE))
(define TOTAL-HEIGHT (* HEIGHT BLOCK-SIZE))

(define dimensions (make-posn (* WIDTH BLOCK-SIZE) (* HEIGHT BLOCK-SIZE)))

(define (make-grid WIDTH HEIGHT BLOCK-SIZE)
  ;;(repeat 1 11 (lambda (index) (add-line vertical? index BLOCK-SIZE image)))
  WIDTH
  )

;;my-range : number number function -> list
;;  returns a list of numbers that have been passed
;;  through the function and iterated upon
;;
;;example : (my-range 1 5 (lambda (n) (* n 10))) -> (list 10 20 30 40 50)

(define (my-range start end f)
  (cond
    [(>= start end) (cons (f start) empty)]
    [else (cons (f start) (my-range (+ start 1) end f))])
  )

(define gridX (my-range 1 (- WIDTH 1) (lambda (n) (* n BLOCK-SIZE))))
(define gridY (my-range 1 (- HEIGHT 1) (lambda (n) (* n BLOCK-SIZE))))

;;create-grid-image : list list grid-size-> image
;;  returns a grid image based on lists given
;;

(define (create-grid-image x y width height W H)
  (cond
    [(empty? y) (rectangle width height "solid" "white")]
    [(empty? x) (add-line (create-grid-image x (rest y) height width W H) 0 (first y) W (first y) "black")]
    [else (add-line (create-grid-image (rest x) y height width W H) (first x) 0 (first x) H "black")])
  )

(define grid-image (create-grid-image gridX gridY WIDTH HEIGHT TOTAL-WIDTH TOTAL-HEIGHT))

;;repeat : Number Number Function -> Number
;;  iterates based on number from (in-range start end)
;;
;;(repeat 0 10 (lambda (n) (* n 1))) -> 0 1 2 3 4 5 6 7 8 9

(define (repeat start end f)
  (cond
    [(>= start (- end 1)) (f start)]
    [else (begin (f start) (repeat (+ 1 start) end f))])
  )

;;join-lists: list list -> list
;;  joins two lists
;;
;;examples: (join-lists (list 1 543 5 23 2 2) (list 2 54 3 3 2 2 22222 2 43 5)) -> (list 1 543 5 23 2 2 2 54 3 3 2 2 22222 2 43 5)

(define (join-lists list1 list2)
  (cond
    [(empty? list2) empty]
    [(empty? list1) (cons (first list2) (join-lists list1 (rest list2)))]
    [else (cons (first list1) (join-lists (rest list1) list2))]
    )
  )

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

(define-struct tetra (color center center-corner? blocks))

(define O-tetra (make-tetra "green" (make-posn 1 -1) true
                 (list (make-posn 0 0) (make-posn 1 0) (make-posn 0 -1) (make-posn -1 -1))))
;; the o block, which will be green, its 

(define L-tetra (make-tetra "purple" (make-posn 1 -1) false
                 (list (make-posn 0 -1) (make-posn 1 -1) (make-posn 2 -1) (make-posn 2 0))))

(define J-tetra (make-tetra "cyan" (make-posn 1 -1) false
                 (list (make-posn 0 -1) (make-posn 1 -1) (make-posn 2 -1) (make-posn 0 0))))

(define I-tetra (make-tetra "darkblue" (make-posn 2 -1) true
                 (list (make-posn 0 0) (make-posn 1 0) (make-posn 2 0) (make-posn 3 0))))

(define T-tetra (make-tetra "orange" (make-posn 1 -1) false
                 (list (make-posn 0 -1) (make-posn 1 -1) (make-posn 1 0) (make-posn 2 -1))))

(define Z-tetra (make-tetra "pink" (make-posn 1 -1) false
                 (list (make-posn 0 0) (make-posn 1 0) (make-posn 1 -1) (make-posn 2 -1))))

(define S-tetra (make-tetra "red" (make-posn 1 -1) false
                 (list (make-posn 0 -1) (make-posn 1 -1) (make-posn 1 0) (make-posn 2 0))))

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

(define (draw w)
  grid-image
  )

(define (key k w)
  k
  )

(define (tick w) w)

;;falalalalala

(big-bang 0
          [on-tick tick]
          [on-draw draw]
          [on-key key])

;;TEST SECTION

;;my-range test

(check-expect (my-range 1 5 (lambda (n) (* n 40))) (list 40 80 120 160 200))

(check-expect (my-range -1 -100 (lambda (n) (* n 40))) (list -40))

;;join-lists test

(check-expect (join-lists (list 1 543 5 23 2 2) (list 2 54 3 3 2 2 22222 2 43 5)) (list 1 543 5 23 2 2 2 54 3 3 2 2 22222 2 43 5))
