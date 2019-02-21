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
;;btw i like yeet and yeeeeeeetn and beaaananana a a  a a a a aye e e  et ie t etieiuiusiusi aiu aiu aij askj s shhhhhhhhhhhhhhhhhhhhheh

;;thx ms jafari

;;World Struct
;; contains the matrix, the score, and the active tetra
;; (the one that is falling)

(require 2htdp/universe )
(require 2htdp/image)

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
;;  returns a list of numbers that have been passed77
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
    [else (begin (f start) (repeat (+ 1 start) end f))]))
;; this has begin, though we don't use this function yet


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

;recur : start end f 

                 

;; Tetra block : Struct which contains the color of the tetra
;; the center of the tetra in which it rotates, and the rotation mode;
;; if the rotation point is at a corner of the block or if it is 

(define-struct tetra (color center center-corner? blocks))

(define O-tetra (make-tetra "green" (make-posn 0.5 0.5) true
                            (list (make-posn 0 0) (make-posn 1 0) (make-posn 0 1) (make-posn 1 1))))
;; the o block, which will be green, its 

(define L-tetra (make-tetra "purple" (make-posn 1 1) false
                            (list (make-posn 0 1) (make-posn 1 1) (make-posn 2 1) (make-posn 2 0))))

(define J-tetra (make-tetra "cyan" (make-posn 1 1) false
                            (list (make-posn 0 1) (make-posn 1 1) (make-posn 2 1) (make-posn 0 0))))

(define I-tetra (make-tetra "darkblue" (make-posn 1.5 0.5) true
                            (list (make-posn 0 0) (make-posn 1 0) (make-posn 2 0) (make-posn 3 0))))

(define T-tetra (make-tetra "orange" (make-posn 1 1) false
                            (list (make-posn 0 1) (make-posn 1 1) (make-posn 1 0) (make-posn 2 1))))

(define Z-tetra (make-tetra "pink" (make-posn 1 1) false
                            (list (make-posn 0 0) (make-posn 1 0) (make-posn 1 1) (make-posn 2 1))))

(define S-tetra (make-tetra "red" (make-posn 1 1) false
                            (list (make-posn 0 1) (make-posn 1 1) (make-posn 1 0) (make-posn 2 0))))

;;
;;BIG BANG STUFF
;;

(define-struct world (matrix active-tetra score interval time))

;;draw-tetra : world image -> image
;;  adds the active tetra to the scene
;;

#;(define (draw-tetra w image)
    ()
    )

;;move-tetra-x: tetra Number -> yeeeneenneneneneneeeeeeeeenr (a.k.a. tetra)
;;  moves tetra laterally
;;
;;

;;center-tetra : tetra
;;
;;

(define (move-tetra t posn-amnt)
  (make-tetra (tetra-color t) (posn-add (tetra-center t) posn-amnt) (tetra-center-corner? t) (add-posn-list (tetra-blocks t) posn-amnt)))

;;add-posn-list: listofposns posn -> list
;;  adds posn to each posn of list
;;
;;(list (posn 10 1) (posn 2 2)) (posn 0 0) -> (list (posn 10 1) (posn 2 2))

(define (add-posn-list list my-posn)
  (cond
    [(empty? list) empty]
    [else (cons (posn-add (first list) my-posn) (add-posn-list (rest list) my-posn))]))

;;posn-add : posn posn -> posn
;;  takes posns and adds them together
;;
;; (posn 1 0) (posn 0 0) -> (posn 1 0)

(define (posn-add p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2)))
  )

;;make-tetra-image : tetra -> image
;;  makes an image representation of a tetra
;;

(define p2 (make-pen "black" 2 "solid" "round" "round"))

(define (make-tetra-image t image)
  (cond
    [(empty? (tetra-blocks t)) image]
    [else (place-image (overlay (crop 0 0 BLOCK-SIZE BLOCK-SIZE (rectangle BLOCK-SIZE BLOCK-SIZE "outline" p2)) (square BLOCK-SIZE "solid" (tetra-color t)))
                       (posn-x (grid->coord (first (tetra-blocks t)) BLOCK-SIZE))
                       (posn-y (grid->coord (first (tetra-blocks t)) BLOCK-SIZE))
                       (make-tetra-image
                        (make-tetra (tetra-color t)
                                    (tetra-center t)
                                    (tetra-center-corner? t)
                                    (rest (tetra-blocks t))) image))]))

(define (grid->coord pos size)
  (make-posn (+ (* (posn-x pos) size) (/ size 2)) (+ (* (posn-y pos) size) (/ size 2))))

;; Draw Function : world -> image
;;  given worldstate, returns an image
;;

(define (draw w)
  ;;(draw-tetra w grid-image)
  (make-tetra-image (world-active-tetra w) grid-image))

 
(define (block-rotate-ccw c b)
  (make-posn (+ (posn-x c)
                 (- (posn-y c)
                    (posn-y b)))
              (+ (posn-y c)
                 (- (posn-x b)
                    (posn-x c)))))

;;rotate-tetra: tetra -> tetra
;;  rotates tetra ccw by 90 degrees
;;
;;(make-tetra "green" (make-posn 1 1) true (list (make-posn 0 0) (make-posn 1 0) (make-posn 0 1) (make-posn 1 1))) -> (make-tetra "green" (make-posn 1 1) true (list (make-posn 0 0) (make-posn 1 0) (make-posn 0 1) (make-posn 1 1)))

(define (rotate-tetra tetra)
  (make-tetra (tetra-color tetra) (tetra-center tetra) (tetra-center-corner? tetra) (rotate-tetra-blocks (tetra-center tetra) (tetra-blocks tetra))))

;;rotate-tetra-blocks : list of blocks -> returns list of blocks (aka a posns)
;;  turns things counterclockwise by 90 degrees
;;
;;(make-tetra "green" (make-posn 1 1) true (list (make-posn 0 0) (make-posn 1 0) (make-posn 0 1) (make-posn 1 1))) -> (make-tetra "green" (make-posn 1 1) true (list (make-posn 0 0) (make-posn 1 0) (make-posn 0 1) (make-posn 1 1)))

(define (rotate-tetra-blocks center list)
  (cond
    [(empty? list) empty]
    [else (cons (block-rotate-ccw center (first list)) (rotate-tetra-blocks center (rest list)))])
  )

;;key: world key -> world
;;  moves active tetra
;;

(define (key w k)
  (cond
    [(string=? k "right") (change-world-tetra w (move-tetra (world-active-tetra w) (make-posn 1 0)))]
    [(string=? k "left") (change-world-tetra w (move-tetra (world-active-tetra w) (make-posn -1 0)))]
    [(string=? k "down") (change-world-tetra w (move-tetra (world-active-tetra w) (make-posn 0 1)))]
    [(string=? k "up") (change-world-tetra w (rotate-tetra (world-active-tetra w)))] ;;moves counter clockwise
    [(string=? k "s") (change-world-tetra w (rotate-tetra (world-active-tetra w)))]
    [(string=? k "a") (change-world-tetra w (rotate-tetra (rotate-tetra (rotate-tetra (world-active-tetra w)))))]
    [else w]
    )
  )

(define (change-world-tetra world tetra)
  (make-world (world-matrix world) tetra (world-score world) (world-interval world) (world-time world))
  )

;;update-tetra-pos : world -> world
;;  changes tetra position down one
;;
;;

(define (update-tetra-pos w)
  (move-tetra (world-active-tetra w) (make-posn 0 1)))

(define (tick w)
 (make-world (world-matrix w) (cond
                                [(equal? (modulo (world-time w) (world-interval w)) 0) (update-tetra-pos w)]
                                [else (world-active-tetra w)]) (world-score w) (world-interval w) (+ (world-time w) 1))
  )

;;falalalalala

(big-bang (make-world 0 (move-tetra J-tetra (make-posn 3 0)) -10000 24 0)
  [on-tick tick]
  [on-draw draw]
  [on-key key])

;;TEST SECTION

;;my-range test

(check-expect (my-range 1 5 (lambda (n) (* n 40))) (list 40 80 120 160 200))

(check-expect (my-range -1 -100 (lambda (n) (* n 40))) (list -40))

;;join-lists test

(check-expect (join-lists (list 1 543 5 23 2 2) (list 2 54 3 3 2 2 22222 2 43 5)) (list 1 543 5 23 2 2 2 54 3 3 2 2 22222 2 43 5))
