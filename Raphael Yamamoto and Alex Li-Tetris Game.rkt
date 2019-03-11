;; Our Tetris Game 

;; Recursive Template
#;(define (function list)
(cond
[(empty? list) empty]
[else (cons (fist list) (function (rest list)))]))

(require 2htdp/universe)
(require 2htdp/image)

;; Grid
;; a 10 by 20 grid, each block

(define WIDTH 10)
(define HEIGHT 20)

(define BLOCK-SIZE 40) ;; size of individual block in grid

(define TOTAL-WIDTH (* WIDTH BLOCK-SIZE)) ;; Width of end image
(define TOTAL-HEIGHT (* HEIGHT BLOCK-SIZE)) ;;Width of end image

(define dimensions (make-posn (* WIDTH BLOCK-SIZE) (* HEIGHT BLOCK-SIZE)))
;;dimensions of the grid

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


;;join-lists: list list -> list
;;  joins two lists
;;
;;examples: (join-lists (list 1 543 5 23 2 2) (list 2 54 3 3 2 2 22222 2 43 5))
;;-> (list 1 543 5 23 2 2 2 54 3 3 2 2 22222 2 43 5)

(define (join-lists list1 list2)
  (cond
    [(empty? list2) empty]
    [(empty? list1) (cons (first list2) (join-lists list1 (rest list2)))]
    [else (cons (first list1) (join-lists (rest list1) list2))]))

;; Tetra block : Struct which contains the color of the tetra
;; the center of the tetra in which it rotates, and the rotation mode;
;; if the rotation point is at a corner of the block or if it is 

(define-struct tetra (color center center-corner? blocks))

(define O-tetra (make-tetra "green" (make-posn 0.5 0.5) true
                            (list (make-posn 0 0) (make-posn 1 0)
                                  (make-posn 0 1) (make-posn 1 1))))
(define L-tetra (make-tetra "purple" (make-posn 1 1) false
                            (list (make-posn 0 1) (make-posn 1 1)
                                  (make-posn 2 1) (make-posn 2 0))))
(define J-tetra (make-tetra "cyan" (make-posn 1 1) false
                            (list (make-posn 0 1) (make-posn 1 1)
                                  (make-posn 2 1) (make-posn 0 0))))
(define I-tetra (make-tetra "darkblue" (make-posn 1.5 0.5) true
                            (list (make-posn 0 0) (make-posn 1 0)
                                  (make-posn 2 0) (make-posn 3 0))))
(define T-tetra (make-tetra "orange" (make-posn 1 1) false
                            (list (make-posn 0 1) (make-posn 1 1)
                                  (make-posn 1 0) (make-posn 2 1))))
(define Z-tetra (make-tetra "pink" (make-posn 1 1) false
                            (list (make-posn 0 0) (make-posn 1 0)
                                  (make-posn 1 1) (make-posn 2 1))))
(define S-tetra (make-tetra "red" (make-posn 1 1) false
                            (list (make-posn 0 1) (make-posn 1 1)
                                  (make-posn 1 0) (make-posn 2 0))))


;; List of Posn in tetris (LoPit)
;; List of tetras and their indexes (place in the list)

(define LoPit (list (make-posn 0 O-tetra)(make-posn 1 L-tetra)
                    (make-posn 2 J-tetra)(make-posn 3 I-tetra)
                    (make-posn 4 T-tetra)(make-posn 5 Z-tetra)
                    (make-posn 6 S-tetra)))


;; tetra-by-number: Number LoPit-> Tetra
;; it takes in a number and returns a tetra
;;
;; example: (tetra-by-number 1)-> O-tetra

(define (tetra-by-number num list)
  (cond
    [(empty? list) (error "array index out of bounds")]
    [(equal? num (posn-x (first list))) (posn-y (first list))]
    [else (tetra-by-number num (rest list))]))

;;GRID MANAGEMENT
;;

(define gridX (my-range 1 (- WIDTH 1) (lambda (n) (* n BLOCK-SIZE))))
;;x positions of grid lines
(define gridY (my-range 1 (- HEIGHT 1) (lambda (n) (* n BLOCK-SIZE))))
;;y positions of grid lines

;;create-grid-image : list list grid-size-> image
;;  returns a grid image based on lists given
;;

(define (create-grid-image x y width height W H)
  (cond
    [(empty? y) (rectangle width height "solid" "white")]
    [(empty? x) (add-line (create-grid-image x (rest y) height width W H) 0
                          (first y) W (first y) "black")]
    [else (add-line (create-grid-image (rest x) y height width W H) (first x) 0
                    (first x) H "black")]))

(define grid-image
  (create-grid-image gridX gridY WIDTH HEIGHT TOTAL-WIDTH TOTAL-HEIGHT))

;;BIG BANG STUFF
;;

;; Block Struct
;; contains a color and position
(define-struct block (color posn))


;;World Struct
;; contains the matrix, the score, and the active tetra
;; (the one that is falling)

(define-struct world (blocks active-tetra score interval time))

;;move-tetra: tetra posn -> tetra
;;  moves a tetra's blocks and center point by given amount
;;
;;(move-tetra (make-tetra "green" (make-posn 0.5 0.5) true
;;                            (list (make-posn 0 0) (make-posn 1 0)
;;                               (make-posn 0 1)
;;                                    (make-posn 1 1))) (make-posn 0 0)) ->
;;(make-tetra "green" (make-posn 0.5 0.5) true
;;                          (list (make-posn 0 0) (make-posn 1 0)
;;                               (make-posn 0 1) (make-posn 1 1)))

(define (move-tetra t posn-amnt)
  (make-tetra (tetra-color t)
              (posn-add (tetra-center t) posn-amnt)
              (tetra-center-corner? t)
              (add-posn-list (tetra-blocks t) posn-amnt)))

;;add-posn-list: listofposns posn -> list
;;  adds posn to each posn of list
;;
;;(list (posn 10 1) (posn 2 2)) (posn 0 0) -> (list (posn 10 1) (posn 2 2))

(define (add-posn-list list my-posn)
  (cond
    [(empty? list) empty]
    [else (cons (posn-add (first list) my-posn)
                (add-posn-list (rest list) my-posn))]))

;;posn-add : posn posn -> posn
;;  takes posns and adds them together
;;
;; (posn 1 0) (posn 0 0) -> (posn 1 0)

(define (posn-add p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2))))


(define p2 (make-pen "black" 2 "solid" "round" "round"))
;;pen for drawing inside blocks

;;grid->coord : posn number -> posn
;;  turns grid coordinates into image coordinates
;;
;;(grid->coord )

(define (grid->coord pos size)
  (make-posn (+ (* (posn-x pos) size) (/ size 2))
             (+ (* (posn-y pos) size) (/ size 2))))

;;make-tetra-image : tetra -> image
;;  makes an image representation of a tetra
;;

(define (make-tetra-image t image)
  (cond
    [(empty? (tetra-blocks t)) image]
    [else (place-image (overlay
                        (crop 0 0 BLOCK-SIZE BLOCK-SIZE
                              (rectangle BLOCK-SIZE BLOCK-SIZE "outline" p2))
                        (square BLOCK-SIZE "solid" (tetra-color t)))
                       (posn-x
                        (grid->coord (first (tetra-blocks t)) BLOCK-SIZE))
                       (posn-y
                        (grid->coord (first (tetra-blocks t)) BLOCK-SIZE))
                       (make-tetra-image
                        (make-tetra (tetra-color t)
                                    (tetra-center t)
                                    (tetra-center-corner? t)
                                    (rest (tetra-blocks t))) image))]))


;; make-block-images: world image-> image
;; takes in a worldstate and an image and returns an image

(define (make-blocks-image w i)
  (cond
    [(empty? (world-blocks w)) i]
    [else (make-blocks-image (make-world (rest (world-blocks w)) (world-active-tetra w)
                                         (world-score w) (world-interval w)
                                         (world-time w))
                             (place-image
                              (overlay
                        (crop 0 0 BLOCK-SIZE BLOCK-SIZE
                              (rectangle BLOCK-SIZE BLOCK-SIZE "outline" p2))
                        (square BLOCK-SIZE "solid" (block-color (first (world-blocks w))))) (posn-x (grid->coord (block-posn (first (world-blocks w))) BLOCK-SIZE)) (posn-y (grid->coord (block-posn (first (world-blocks w))) BLOCK-SIZE)) i))]))




;; Draw Function : world -> image
;;  given worldstate, returns an image
;;

(define (draw w)
(make-tetra-image (world-active-tetra w) (make-blocks-image w grid-image)))


;;block-rotate-ccw: Posn Block-> Block
;; takes in a posn and a block and rotates the block by
;; counterclockwise 90 degrees (around the posn)

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
;;(make-tetra "green" (make-posn 1 1) true (list (make-posn 0 0)
;;(make-posn 1 0) (make-posn 0 1) (make-posn 1 1)))
;;-> (make-tetra "green" (make-posn 1 1) true (list (make-posn 0 0)
;;(make-posn 1 0) (make-posn 0 1) (make-posn 1 1)))

(define (rotate-tetra tetra)
  (make-tetra (tetra-color tetra)
              (tetra-center tetra)
              (tetra-center-corner? tetra)
              (rotate-tetra-blocks (tetra-center tetra) (tetra-blocks tetra))))

;;rotate-tetra-blocks : list of blocks -> returns list of blocks (aka a posns)
;;  turns things counterclockwise by 90 degrees
;;
;;(make-tetra "green" (make-posn 1 1) true (list (make-posn 0 0)
;;(make-posn 1 0) (make-posn 0 1) (make-posn 1 1)))
;;-> (make-tetra "green" (make-posn 1 1) true (list (make-posn 0 0)
;;(make-posn 1 0) (make-posn 0 1) (make-posn 1 1)))

(define (rotate-tetra-blocks center list)
  (cond
    [(empty? list) empty]
    [else (cons
           (block-rotate-ccw center (first list))
           (rotate-tetra-blocks center (rest list)))]))

;;in-left? : tetra Number -> boolean
;;  if tetra is inside the left side of the image, return true
;;
;;(I-tetra) -> true

(define (in-left? tetra width)
  (cond
    [(empty? (tetra-blocks tetra)) true]
    [(<= (posn-x (first (tetra-blocks tetra))) 0) false]
    [else (in-left? (make-tetra (tetra-color tetra)
                                (tetra-center tetra)
                                (tetra-center-corner? tetra)
                                (rest (tetra-blocks tetra))) width)]))

;;in-right? : tetra Number -> boolean
;;  if tetra is inside the right side of the image, return true
;;
;;(I-tetra) -> true

(define (in-right? tetra width)
  (cond
    [(empty? (tetra-blocks tetra)) true]
    [(>= (posn-x (first (tetra-blocks tetra))) (- width 1)) false]
    [else (in-right? (make-tetra (tetra-color tetra)
                                 (tetra-center tetra)
                                 (tetra-center-corner? tetra)
                                 (rest (tetra-blocks tetra))) width)]))

;;key: world key -> world
;;  moves active tetra
;;
;;(color center center-corner? blocks

(define (key w k)
  (cond
    [(string=? k "right")
     (change-world-tetra w (move-tetra (world-active-tetra w)
                                       (if
                                        (in-right? (world-active-tetra w) WIDTH)
                                        (make-posn 1 0) (make-posn 0 0))))]
    [(string=? k "left")
     (change-world-tetra w (move-tetra (world-active-tetra w)
                                       (if
                                        (in-left? (world-active-tetra w) WIDTH)
                                        (make-posn -1 0) (make-posn 0 0))))]
    [(string=? k "down")
     (update-tetra-pos w)]
    [(string=? k "up")
     (change-world-tetra w (rotate-tetra (world-active-tetra w)))]
    ;;moves counter clockwise
    [(string=? k "s")
     (change-world-tetra w (rotate-tetra (world-active-tetra w)))]
    [(string=? k "a")
     (change-world-tetra w (rotate-tetra
                            (rotate-tetra
                             (rotate-tetra (world-active-tetra w)))))]
    ;; to turn clockwise
    [else w]))  

;;change-world-tetra : world tetra -> world
;;  changes only the world tetra
;;

(define (change-world-tetra world tetra)
  (make-world
   (world-blocks world)
   tetra (world-score world)
   (world-interval world)
   (world-time world)))


;; Tetra->Block
;; takes in a tetra and returns a block
;;
;;example: (O-tetra)->(list (block "green" (posn 1 0)))

(define (tetra->blocks te)
  (cond
    [(empty? (tetra-blocks te)) empty]
    [else (cons (make-block
                 (tetra-color te) (first (tetra-blocks te)))
                (tetra->blocks (make-tetra (tetra-color te)
                                           (tetra-center te)
                                           (tetra-center-corner? te)
                                           (rest (tetra-blocks te)))))]))


(define (lowest-posn-list my-list val)
  (cond
    [(empty? my-list) val]
    [(> (posn-y (first my-list)) (posn-y val)) (lowest-posn-list
                                                (rest my-list) (first my-list))]
    [else (lowest-posn-list (rest my-list) val)]))

(define (bottom? t)
  (>= (posn-y (lowest-posn-list (tetra-blocks t)
                                (make-posn -inf.0 -inf.0)))
      (- HEIGHT 1))) ;; checks if the tetra has hit the bottom, unfinished

;;update-tetra-pos : world -> world
;;  changes tetra position down one
;;
;;

(define (update-tetra-pos w)
  (cond
    [(bottom? (world-active-tetra w))
     (make-world (join-lists (world-blocks w)
                             (tetra->blocks (world-active-tetra w)))
                 (move-tetra
                       (tetra-by-number (random 0 6) LoPit)
                       (make-posn 3 0)) (world-score w)
                 (world-interval w) (+ (world-time w) 1))] 
    [else (make-world (world-blocks w)(move-tetra (world-active-tetra w)
                                                  (make-posn 0 1))
                      (world-score w) (world-interval w) (+ (world-time w) 1))]))


 
;;tick : world -> world
;; every x frames, update the tetra position
;;

(define (tick w)
  (cond
   [(equal? (modulo (world-time w) (world-interval w)) 0) (update-tetra-pos w)]
   [else (make-world (world-blocks w) (world-active-tetra w)
                     (world-score w)
                     (world-interval w) (+ (world-time w) 1))]))

(big-bang (make-world empty
                      (move-tetra
                       (tetra-by-number 0 LoPit)
                       (make-posn 4 0)) -10000 24 0)
  [on-tick tick]
  [on-draw draw]
  [on-key key])

;;TEST SECTION

;;my-range test

(check-expect (my-range 1 5 (lambda (n) (* n 40))) (list 40 80 120 160 200))

(check-expect (my-range -1 -100 (lambda (n) (* n 40))) (list -40))

;;join-lists test

(check-expect (join-lists
               (list 1 543 5 23 2 2)
               (list 2 54 3 3 2 2 22222 2 43 5))
              (list 1 543 5 23 2 2 2 54 3 3 2 2 22222 2 43 5))

;;draw test
(check-expect (draw (make-world empty
                      (move-tetra
                       (tetra-by-number 0 LoPit)
                       (make-posn 2 0)) -1000 24 0))
                      (scale 10 .))

#;(check-expect (scale 0.1 (draw (make-world empty
                      (move-tetra
                       (tetra-by-number 1 LoPit)
                       (make-posn 4 0)) -1000 24 0)))
                (scale 10 .))


(check-expect (draw (make-world empty
                      (move-tetra
                       (tetra-by-number 2 LoPit)
                       (make-posn 4 0)) -1000 24 0))
              (scale 10 .))

(check-expect (draw (make-world empty
                      (move-tetra
                       (tetra-by-number 3 LoPit)
                       (make-posn 4 0)) -1000 24 0))
              (scale 10 .))
(check-expect (draw (make-world empty
                      (move-tetra
                       (tetra-by-number 4 LoPit)
                       (make-posn 4 0)) -1000 24 0))
              (scale 10 .))
(check-expect (draw (make-world empty
                      (move-tetra
                       (tetra-by-number 5 LoPit)
                       (make-posn 3 0)) -10000 24 0))
              (scale 10 .))
(check-expect (draw (make-world empty
                      (move-tetra
                       (tetra-by-number 6 LoPit)
                       (make-posn 4 0)) -1000 24 0))
              (scale 10 .))
;;tetra-by-number tests

(check-expect (tetra-by-number 0 LoPit) O-tetra)
(check-expect (tetra-by-number 1 LoPit) L-tetra)
(check-expect (tetra-by-number 2 LoPit) J-tetra)
(check-expect (tetra-by-number 3 LoPit) I-tetra)
(check-expect (tetra-by-number 4 LoPit) T-tetra)
(check-expect (tetra-by-number 5 LoPit) Z-tetra)
(check-expect (tetra-by-number 6 LoPit) S-tetra)
(check-expect (tetra-by-number (list empty) LoPit)(or(error "array index out of bounds"))


;; tetra->blocks
(check-expect (tetra->blocks O-tetra) (list (make-block "green" (make-posn 0 0))
                                            (make-block "green" (make-posn 1 0))
                                            (make-block "green" (make-posn 0 1))
                                          (make-block "green" (make-posn 1 1))))

(check-expect (tetra->blocks S-tetra) (list (make-block "red" (make-posn 0 1))
                                            (make-block "red" (make-posn 1 1))
                                            (make-block "red" (make-posn 1 0))
                                            (make-block "red" (make-posn 2 0))))

;;;update-tetra-pos
(check-expect (update-tetra-pos (make-world empty
                (move-tetra
                 (tetra-by-number 5 LoPit)
                  (make-posn 3 0)) -10000 24 0)) (make-world empty (make-tetra "pink" (make-posn 4 2) false
                            (list (make-posn 3 1) (make-posn 4 1)
                                  (make-posn 4 2) (make-posn 5 2))) -10000 24 1))
;; block-rotate-ccw
;; (check-expect (block-rotate-ccw
