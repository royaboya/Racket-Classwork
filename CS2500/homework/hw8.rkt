#lang racket

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction.

(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-TR (make-pipe #true #false #false #true))
(define PIPE-BL (make-pipe #false #true #true #false))
(define PIPE-BR (make-pipe #false #true #false #true))
(define PIPE-TB (make-pipe #true #true #false #false))
(define PIPE-LR (make-pipe #false #false #true #true))
(define PIPE-CROSS (make-pipe #true #true #true #true))
(define PIPE-START-TOP (make-pipe #true #false #false #false))
(define PIPE-START-BOTTOM (make-pipe #false #true #false #false))
(define PIPE-START-LEFT (make-pipe #false #false #true #false))
(define PIPE-START-RIGHT (make-pipe #false #false #false #true))

(define ALL-PIPES (list PIPE-TL PIPE-TR PIPE-BL PIPE-BR PIPE-TB PIPE-LR PIPE-CROSS))

(define (pipe-template p)
  (...(pipe-top p)...(pipe-bot p)...(pipe-left p)...(pipe-right p))) 


;; pipe->image: Pipe Integer Integer -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length
(define (pipe->image pipe tile-side-length pipe-width filled?)
  (local [ ;; gray frame of pipe
          (define PS (square tile-side-length "solid" "gray"))

          ;; Pipe color based on filled?
          (define (pipe-col fill) (if fill "green" "black"))
          (define PIPE-COLOR (pipe-col filled?))

          ;; Vertical and Horizontal Components
          (define P-VERTICAL (rectangle pipe-width (* 1/2 tile-side-length) "solid" PIPE-COLOR))
          (define P-HORIZONTAL (rectangle (+ (* 1/2 pipe-width) (* 1/2 tile-side-length)) pipe-width "solid" PIPE-COLOR))
          ]
    (cond
      ;; cross
      [(and (pipe-top pipe) (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe))
       (overlay/align "middle" "center" (rectangle pipe-width tile-side-length "solid" PIPE-COLOR)
                      (overlay/align "middle" "center" (rectangle tile-side-length pipe-width "solid" PIPE-COLOR) PS))]
      
      ;; top left
      [(and (pipe-top pipe) (pipe-left pipe)) (overlay/align "middle" "top" P-VERTICAL
                                                             (overlay/align "left" "center" P-HORIZONTAL PS))]
      ;; top right
      [(and (pipe-top pipe) (pipe-right pipe)) (overlay/align "middle" "top" P-VERTICAL
                                                              (overlay/align "right" "center" P-HORIZONTAL PS))]
      ;; bot left
      [(and (pipe-bot pipe) (pipe-left pipe)) (overlay/align "middle" "bottom" P-VERTICAL
                                                             (overlay/align "left" "center" P-HORIZONTAL PS))]
      ;; bot right
      [(and (pipe-bot pipe) (pipe-right pipe)) (overlay/align "middle" "bottom" P-VERTICAL
                                                              (overlay/align "right" "center" P-HORIZONTAL PS))]
      ;; top bottom (straight)
      [(and (pipe-top pipe) (pipe-bot pipe)) (overlay/align "middle" "center"
                                                            (rectangle pipe-width tile-side-length "solid" PIPE-COLOR) PS)]
      ;; left right (across straight)
      [(and (pipe-left pipe) (pipe-right pipe))(overlay/align "middle" "center"
                                                              (rectangle tile-side-length pipe-width "solid" PIPE-COLOR) PS)]
    
      ;; start top
      [(pipe-top pipe) (overlay/align "middle" "top"
                                      (rectangle pipe-width (* 2/3 tile-side-length) "solid" PIPE-COLOR) PS)]
      ;; start bottom
      [(pipe-bot pipe) (overlay/align "middle" "bottom"
                                      (rectangle pipe-width (* 2/3 tile-side-length)  "solid" PIPE-COLOR) PS)]
      ;; start left
      [(pipe-left pipe) (overlay/align "left" "center"
                                       (rectangle (* 2/3 tile-side-length) pipe-width "solid" PIPE-COLOR) PS)]
      ;; start right
      [(pipe-right pipe) (overlay/align "right" "center"
                                        (rectangle (* 2/3 tile-side-length) pipe-width "solid" PIPE-COLOR) PS)])))

(define-struct placed-pipe [pipe row col fill]) 
;; A Placed-Pipe is a (make-placed-pipe Pipe Number Number)
;; Represents a placed pipe on a Grid where:
;; - Pipe is the type of pipe being placed
;; - row is the row number of the placed pipe
;; - column is the column number of the placed pipe

(define (placed-pipe-template p)
  (...(placed-pipe-pipe p)...(placed-pipe-row p)...(placed-pipe-col p)...))

;; EXAMPLES
(define P1 (make-placed-pipe PIPE-TL 1 2 #f))
(define P2 (make-placed-pipe PIPE-TB 3 1 #f))
(define P3 (make-placed-pipe PIPE-CROSS 2 2 #f))

(define-struct grid [dimension pipes])
;; A Grid is a (make-grid number [List of Placed-Pipe])
;; Represents a grid of pipes where:
;; dimension is the number of rows and columns for the n x n grid
;; pipes is the list of placed pipes in the grid

(define (grid-template g)
  (...(grid-dimension grid)...(grid-pipes g)...))


(define-struct goo-flow[top bot left right path])
;; A GooFlow is a (make-goo-flow Boolean Boolean Boolean Boolean [List-of Posn])
;; - One of top, bot, left, right are #true while the rest are #false, indicating the
;; direction of goo flow
;; - path is a list of positions for the goo to propogate through
;; Represents a direction and a stored path for goo to flow through

(define (goo-flow-temp gf)
  (...(goo-flow-top gf)...(goo-flow-bot gf)...
      (goo-flow-left gf)...(goo-flow-right)...(goo-flow-path gf)))

(define GOO-1 (make-goo-flow #f #f #f #t (list (make-posn 0 0))))
(define GOO-2 (make-goo-flow #t #f #f #f (list (make-posn 1 3))))
(define GOO-3 (make-goo-flow #t #f #f #f (list (make-posn 2 4))))

;; grid-goo-propagate: GooFlow Grid -> GooFlow
;; Moves the goo forward by one tile, same if unapplicable
(define (grid-goo-propagate gf grid)
  (local[
         (define NEXT-POS (next-posn gf))
         ]
    (cond ;; gf will start with a start position upon init
      [(not (pipe? (pipe-at grid (posn-x NEXT-POS) (posn-y NEXT-POS)))) gf]

      ;; just in case somehow not pipe? fails in which it shouldnt since pipes *shouldnt* exist out of bounds
      [(or (< (posn-x NEXT-POS) 0) (< (posn-y NEXT-POS) 0)
           (> (posn-x NEXT-POS) (grid-dimension grid)) (> (posn-y NEXT-POS) (grid-dimension grid))) gf]

      [(boolean? (next-goo grid gf)) gf]
      [else (next-goo grid gf)])))


;; next-posn: GooFlow -> Posn
;; Calculates next possible position from a goo-flow
(define (next-posn gf)
  (local[
         (define CURR (list-ref (goo-flow-path gf) (sub1 (length (goo-flow-path gf)))))
         ]
    (cond
      [(empty? (goo-flow-path gf)) (error "list is empty, should never be empty for posn calculation")]
      [(goo-flow-top gf) (make-posn (posn-x CURR) (- (posn-y CURR) 1))]
      [(goo-flow-bot gf) (make-posn (posn-x CURR) (+ (posn-y CURR) 1))]
      [(goo-flow-left gf) (make-posn (- (posn-x CURR) 1) (posn-y CURR))]
      [(goo-flow-right gf) (make-posn (+ (posn-x CURR) 1) (posn-y CURR))])))

(check-expect (next-posn (make-goo-flow #t #f #f #f (list (make-posn 0 1)))) (make-posn 0 0))
(check-expect (next-posn (make-goo-flow #f #t #f #f (list (make-posn 0 1)))) (make-posn 0 2))
(check-expect (next-posn (make-goo-flow #f #f #t #f (list (make-posn 0 1)))) (make-posn -1 1))
(check-expect (next-posn (make-goo-flow #f #f #f #t (list (make-posn 0 1)))) (make-posn 1 1))


;; next-goo: Grid GooFlow -> GooFlow
;; Calculates Goo Flow Direction for next Pipe, evaluates #false if next pipe is an incompatible pipe 
(define (next-goo grid gf)
  (local[
         (define p (next-posn gf))
         (define NEXT-PIPE (pipe-at grid (posn-x p) (posn-y p)))
         ;; if top bottom does not cross, then check left and right
         (define (top/bot pipe)
           (cond
             [(pipe-left pipe) (make-goo-flow #f #f #t #f (append (goo-flow-path gf) (list p)))]
             [(pipe-right pipe) (make-goo-flow #f #f #f #t (append(goo-flow-path gf) (list p)))]
             ))
         ;; if left right does not cross, then check top and left
         (define (left/right pipe)
           (cond
             [(pipe-top pipe) (make-goo-flow #t #f #f #f (append(goo-flow-path gf) (list p)))]
             [(pipe-bot pipe) (make-goo-flow #f #t #f #f (append(goo-flow-path gf) (list p)))]
             ))
         ]
    (cond

      ;; if pipe does not exist
      [(boolean? NEXT-PIPE) #f]

      ;; if can cross the pipe in one direction
      [(and (goo-flow-top gf) (pipe-top NEXT-PIPE) (pipe-bot NEXT-PIPE)) (make-goo-flow #t #f #f #f (append (goo-flow-path gf) (list p)))] 
      [(and (goo-flow-bot gf) (pipe-top NEXT-PIPE) (pipe-bot NEXT-PIPE)) (make-goo-flow #f #t #f #f (append (goo-flow-path gf) (list p)))] 
      [(and (goo-flow-left gf) (pipe-right NEXT-PIPE) (pipe-left NEXT-PIPE)) (make-goo-flow #f #f #t #f (append (goo-flow-path gf) (list p)))]
      [(and (goo-flow-right gf) (pipe-right NEXT-PIPE) (pipe-left NEXT-PIPE)) (make-goo-flow #f #f #f #t (append(goo-flow-path gf) (list p)))]

      ;; if pipes are not compatible
      [(and (goo-flow-right gf) (not (pipe-left NEXT-PIPE))) gf]
      [(and (goo-flow-left gf) (not (pipe-right NEXT-PIPE))) gf]
      [(and (goo-flow-top gf) (not (pipe-bot NEXT-PIPE))) gf]
      [(and (goo-flow-bot gf) (not (pipe-top NEXT-PIPE))) gf]
      
      ;; if direction needs to be changed
      [(goo-flow-top gf) (top/bot NEXT-PIPE)]
      [(goo-flow-bot gf) (top/bot NEXT-PIPE)]
      [(goo-flow-left gf) (left/right NEXT-PIPE)]
      [(goo-flow-right gf) (left/right NEXT-PIPE)])))

 
(define STARTING-GRID (make-grid 7 '()))

;; More examples
(define GRID-1 (make-grid 6 (list (make-placed-pipe PIPE-TL 1 2 #f))))
(define GRID-2 (make-grid 3 (list (make-placed-pipe PIPE-TL 1 1 #f))))
(define GRID-3 (make-grid 9 (list (make-placed-pipe PIPE-TL 3 3 #f))))


;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.
(define (place-pipe grid pipe row col fill)
  (make-grid (grid-dimension grid) ( cons (make-placed-pipe pipe row col fill) (grid-pipes grid))))

;; check expects:
(check-expect (place-pipe STARTING-GRID PIPE-TL 1 1 #f) (make-grid 7 (list (make-placed-pipe PIPE-TL 1 1 #f))))

(check-expect (place-pipe GRID-1 PIPE-TL 1 2 #f) (make-grid 6 (list (make-placed-pipe PIPE-TL 1 2 #f) (make-placed-pipe PIPE-TL 1 2 #f))))

(check-expect (place-pipe GRID-2 PIPE-TR 2 4 #f) (make-grid 3 (list (make-placed-pipe PIPE-TR 2 4 #f) (make-placed-pipe PIPE-TL 1 1 #f))))

;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.
(define (pipe-at grid row col) ;; row is y, col is x
  (cond
    [(empty? (grid-pipes grid)) #false]
    [(and (= row (placed-pipe-row (first (grid-pipes grid))))
          (= col (placed-pipe-col (first (grid-pipes grid)))))
     (placed-pipe-pipe (first (grid-pipes grid)))]
    [else (pipe-at (make-grid (grid-dimension grid) (rest (grid-pipes grid))) row col)]))

;; check expects:
(check-expect (pipe-at GRID-1 1 2) PIPE-TL)
(check-expect (pipe-at STARTING-GRID 0 0) #false)
(check-expect (pipe-at GRID-2 1 1) PIPE-TL)
(check-expect (pipe-at GRID-2 3 2) #false)


;; grid->image: Grid Integer Integer -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.
(define (grid->image grid tile-side-length pipe-width incoming)
  (local[
         (define ROW (row-tile tile-side-length (grid-dimension grid)))
         (define GRID (grid-from-row ROW (grid-dimension grid)))
         (define GRID/v2 (above/align "left" GRID (draw-incoming-pipes 4 incoming tile-side-length pipe-width)))
         ]

    (cond
      [(empty? (grid-pipes grid)) GRID/v2]
      [else (overlay/align/offset "left" "top"
                                  (pipe->image
                                   (placed-pipe-pipe (first (grid-pipes grid)))
                                   tile-side-length
                                   pipe-width
                                   (placed-pipe-fill (first (grid-pipes grid))))
                                  (- 0 (* tile-side-length (placed-pipe-row (first (grid-pipes grid)))))
                                  (- 0 (* tile-side-length (placed-pipe-col (first (grid-pipes grid)))))
       
                                  (grid->image (make-grid (grid-dimension grid) (rest (grid-pipes grid))) tile-side-length pipe-width incoming))])))

;; row-tile: Number Number -> Image
;; Creates a 1xn row of tiles for a nxn grid
(define (row-tile tile-side-length n)
  (local[
         (define tile (square tile-side-length "outline" "black"))
         ]
    (cond
      [(= 1 n) tile]
      [else (beside tile (row-tile tile-side-length (- n 1)))])))

;; checkexpects
(check-expect (row-tile 10 2) (beside (square 10 "outline" "black") (square 10 "outline" "black")))
(check-expect (row-tile 10 1) (square 10 "outline" "black"))
(check-expect (row-tile 5 2) (beside (square 5 "outline" "black") (square 5 "outline" "black")))


;; grid-from-row: Image Number -> Image
;; creates a nxn grid from a row of tiles 
(define (grid-from-row row-tiles n)
  (cond
    [(= 1 n) row-tiles]
    [else (above row-tiles (grid-from-row row-tiles (- n 1)))]))

;; checkexpects
(check-expect (grid-from-row (row-tile 10 2) 2) (above (row-tile 10 2) (row-tile 10 2)))
(check-expect (grid-from-row (row-tile 6 2) 4) (above (row-tile 6 2) (row-tile 6 2) (row-tile 6 2) (row-tile 6 2)))
(check-expect (grid-from-row (row-tile 1 2) 2) (above (row-tile 1 2) (row-tile 1 2)))

;; draw-incoming-pipes: Integer [List-of Pipe] Number Number -> Image
;; Draws the n incoming pipes
(define (draw-incoming-pipes n pipe-list tl pw)
  (local[
         ; if list is empty and n != 0, draw blanks
         ; if list not empty, recursively draw pipes
         ; else, stop
         (define EMPTY (square tl "outline" "white"))
         ]
    (cond
      [(and (empty? pipe-list) (not (= 1 n))) (beside EMPTY (draw-incoming-pipes (- n 1) pipe-list tl pw))]
      [(and (not (empty? pipe-list)) (not (= 1 n))) (beside (pipe->image (first pipe-list) tl pw #f)
                                                            (draw-incoming-pipes (- n 1) (rest pipe-list) tl pw))]
      [(and (empty? pipe-list) (= 1 n)) EMPTY]
      [(= 1 n) (pipe->image (first pipe-list) tl pw #f)])))

;; check-expects
(check-expect (draw-incoming-pipes 1 ALL-PIPES 100 30) (pipe->image PIPE-TL 100 30 #f))
(check-expect (draw-incoming-pipes 2 (list PIPE-TR PIPE-TL PIPE-TB) 100 30)
              (beside (pipe->image PIPE-TR 100 30 #f) (pipe->image PIPE-TL 100 30 #f)))
(check-expect (draw-incoming-pipes 3 ALL-PIPES 100 30)
              (beside (pipe->image PIPE-TL 100 30 #f) (pipe->image PIPE-TR 100 30 #f) (pipe->image PIPE-BL 100 30 #f)))



(define-struct game-state [grid pipes-to-place tile-side-length pipe-width start goo-flow])
;; A GameState is a (make-game-state grid [List of Pipes] Number)
;; Represents the current GameState where
;; - grid is the grid to place pipes on
;; - pipes-to-place is a list of available pipes to be placed into the grid
;; - tile-side-length is the length of the tiles
;; - start is (starting pipe or pipe location idk yet) probably pipe
;; - goo-flow is the GooFlow of the game 

;; STATE-ONE is a game state with empty starting grid with some pipes to place 
(define STATE-ONE (make-game-state STARTING-GRID
                                   (list PIPE-TL PIPE-CROSS PIPE-BL PIPE-TL PIPE-CROSS PIPE-TR)
                                   100
                                   30
                                   PIPE-START-TOP
                                   GOO-1))

;; STATE-TWO is a game state with an already placed pipe with some more pipes to place
(define STATE-TWO (make-game-state GRID-1
                                   (list PIPE-CROSS PIPE-CROSS PIPE-CROSS PIPE-BL PIPE-TL)
                                   150
                                   50
                                   PIPE-START-BOTTOM
                                   GOO-2))

;; STATE-THREE is a game state with an already placed pipe with no pipes to place
(define STATE-THREE (make-game-state GRID-3 '
                                     ()
                                     150
                                     50
                                     PIPE-START-RIGHT
                                     GOO-3))

;; A Direction is one of:
;; - "top"
;; - "bottom"
;; - "left"
;; - "right"
;; Represents a direction on a grid or pipe

(define TOP "top")
(define BOTTOM "bottom")
(define LEFT "left")
(define RIGHT "right")

(define (direction-temp d)
  (cond
    [(string=? d TOP)...]
    [(string=? d BOTTOM)...]
    [(string=? d LEFT)...]
    [(string=? d RIGHT)...]))


;; game-state-init: Integer Integer Integer Direction [List-of Pipe] Integer Integer-> GameState
;; Initializes a GameState for a Pipe Game
(define (gamestate-init n x y dir pipe-list tl pw)
  (local[
         (define (dir->start d)
           (cond
             [(string=? TOP d) PIPE-START-TOP]
             [(string=? BOTTOM d) PIPE-START-BOTTOM]
             [(string=? LEFT d) PIPE-START-LEFT]
             [(string=? RIGHT d) PIPE-START-RIGHT]))
         
         (define PIPE-TYPE (dir->start dir)) 

         (define (dir->gooflow d)
           (cond
             [(string=? TOP d) (make-goo-flow #t #f #f #f (list (make-posn x y)))]
             [(string=? BOTTOM d) (make-goo-flow #f #t #f #f (list (make-posn x y)))]
             [(string=? LEFT d) (make-goo-flow #f #f #t #f (list (make-posn x y)))]
             [(string=? RIGHT d) (make-goo-flow #f #f #f #t (list (make-posn x y)))]))
         
         (define g (make-grid n (list (make-placed-pipe (dir->start dir) x y #f))))
         ]
    (make-game-state g pipe-list tl pw PIPE-TYPE (dir->gooflow dir))))

;; game-state-init check-expects
(check-expect (gamestate-init 5 0 0 LEFT '() 100 30) (make-game-state
                     (make-grid 5 (list (make-placed-pipe (make-pipe #false #false #true #false) 0 0 #false)))
                     '()
                     100
                     30
                     (make-pipe #false #false #true #false)
                     (make-goo-flow #false #false #true #false (list (make-posn 0 0)))))

(check-expect (gamestate-init
               2
               0
               0
               BOTTOM
               '() 100 30) (make-game-state
                     (make-grid 2 (list (make-placed-pipe (make-pipe #false #true #false #false) 0 0 #false)))
                     '()
                     100
                     30
                     (make-pipe #false #true #false #false)
                     (make-goo-flow #false #true #false #false (list (make-posn 0 0)))))

(check-expect (gamestate-init
               3
               0
               0
               BOTTOM
               (list PIPE-TR PIPE-LR) 100 30)(make-game-state
                                       (make-grid 3 (list (make-placed-pipe (make-pipe #false #true #false #false) 0 0 #false)))
                                       (list (make-pipe #true #false #false #true) (make-pipe #false #false #true #true))
                                       100
                                       30
                                       (make-pipe #false #true #false #false)
                                       (make-goo-flow #false #true #false #false (list (make-posn 0 0)))))

;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.
(define (place-pipe-on-click gs x y me)
  (local[
         (define SCALING-FACTOR (game-state-tile-side-length gs))
         
         (define (add-filled-pipe gs) (place-pipe (game-state-grid gs) PIPE-START-BOTTOM 0 0 #t))
         
         (define (add-filled-pipe/v2 gs)
           (local[
                  (define grid (game-state-grid gs))
                  (define gf (game-state-goo-flow gs))
                  ]
             (make-game-state
              (place-pipe (game-state-grid gs)
                          (pipe-at grid (posn-x (first (goo-flow-path gf)))
                                   (posn-y (first (goo-flow-path gf))))
                          (posn-x (first (goo-flow-path gf)))
                          (posn-y (first (goo-flow-path gf)))
                          #t)
              (game-state-pipes-to-place gs)
              (game-state-tile-side-length gs)
              (game-state-pipe-width gs)
              (game-state-start gs)
              (make-goo-flow (goo-flow-top gf)
                             (goo-flow-bot gf)
                             (goo-flow-left gf)
                             (goo-flow-right gf)
                             (rest (goo-flow-path gf)))))) 
         ]
    
    (cond
      [(and (empty? (game-state-pipes-to-place gs)) (mouse=? me "button-down")) (add-filled-pipe/v2 (propogate-finish gs))]
      [(or (> (+ 1 (floor (/ y SCALING-FACTOR))) (grid-dimension (game-state-grid gs)))
           (> (+ 1(floor (/ x SCALING-FACTOR))) (grid-dimension (game-state-grid gs)))) gs]
      
      [(empty? (game-state-pipes-to-place gs)) gs]
      
      [(mouse=? me "button-down")
       (make-game-state
        (place-pipe (game-state-grid gs)
                    (first (game-state-pipes-to-place gs))
                    (floor (/ x SCALING-FACTOR)) (floor (/ y SCALING-FACTOR)) #f) 
        (rest (game-state-pipes-to-place gs))
        (game-state-tile-side-length gs)
        (game-state-pipe-width gs)
        (game-state-start gs)
        (game-state-goo-flow gs))]
        ;;(grid-goo-propagate (game-state-goo-flow gs) (game-state-grid gs))
        ;; goo can be propogated here per click using grid-goo-propagate but goo does not move
        ;; on tick for this assignment so it will propagate n times at the end for now
      [else gs])))

(check-expect (place-pipe-on-click (make-game-state
                                    (make-grid 3 (list (make-placed-pipe (make-pipe #false #true #false #false) 0 0 #false)))
                                    (list (make-pipe #true #false #false #true) (make-pipe #false #false #true #true))
                                    100
                                    30
                                    (make-pipe #false #true #false #false)
                                    (make-goo-flow #false #true #false #false (list (make-posn 0 0)))) 50 50 "button-down")
              (make-game-state
               (make-grid 3 (list (make-placed-pipe (make-pipe #true #false #false #true) 0 0 #false) (make-placed-pipe (make-pipe #false #true #false #false) 0 0 #false)))
               (list (make-pipe #false #false #true #true))
               100
               30
               (make-pipe #false #true #false #false)
               (make-goo-flow #false #true #false #false (list (make-posn 0 0)))))
(check-expect (place-pipe-on-click (make-game-state
                                    (make-grid 5 (list (make-placed-pipe (make-pipe #false #false #true #false) 0 0 #false)))
                                    '()
                                    100
                                    30
                                    (make-pipe #false #false #true #false)
                                    (make-goo-flow #false #false #true #false (list (make-posn 0 0)))) 50 50 "button-down")
              (make-game-state
               (make-grid 5 (list (make-placed-pipe (make-pipe #false #false #true #false) 0 0 #true) (make-placed-pipe (make-pipe #false #false #true #false) 0 0 #false)))
               '()
               100
               30
               (make-pipe #false #false #true #false)
               (make-goo-flow #false #false #true #false '())))
(check-expect (place-pipe-on-click (make-game-state
                                    (make-grid 2 (list (make-placed-pipe (make-pipe #false #true #false #false) 0 0 #false)))
                                    '()
                                    100
                                    30
                                    (make-pipe #false #true #false #false)
                                    (make-goo-flow #false #true #false #false (list (make-posn 0 0)))) 50 50 "button-down")
              (make-game-state
               (make-grid 2 (list (make-placed-pipe (make-pipe #false #true #false #false) 0 0 #true) (make-placed-pipe (make-pipe #false #true #false #false) 0 0 #false)))
               '()
               100
               30
               (make-pipe #false #true #false #false)
               (make-goo-flow #false #true #false #false '())))


;; propogate-finish: GameState -> GameState
;; Completely propagates a GooFlow within a GameState
;; This Function can be modified with the difference in lengths of the lists
;; so that goo can be propagated every click
(define (propogate-finish gs)
  (local[
         ;; get number of missing propogations or just do total pipes at a time
         ;; (define N ( - (length (grid-pipes (game-state-grid gs))) (length (goo-flow-path (game-state-goo-flow gs)))))
         (define N (length (grid-pipes (game-state-grid gs))))
         (define (propogate n gs)
           (cond
             [(= 0 n) (make-game-state (game-state-grid gs)
                                       (game-state-pipes-to-place gs)
                                       (game-state-tile-side-length gs)
                                       (game-state-pipe-width gs)
                                       (game-state-start gs)
                                       (grid-goo-propagate (game-state-goo-flow gs) (game-state-grid gs))
                                       )]
             [else (propogate (- n 1) (make-game-state (game-state-grid gs)
                                                       (game-state-pipes-to-place gs)
                                                       (game-state-tile-side-length gs)
                                                       (game-state-pipe-width gs)
                                                       (game-state-start gs)
                                                       (grid-goo-propagate (game-state-goo-flow gs) (game-state-grid gs))))]))]
    (propogate N gs)))

;; check-expects
(check-expect (propogate-finish (make-game-state
                                 (make-grid 3 (list (make-placed-pipe (make-pipe #false #true #false #false) 0 0 #false)))
                                 (list (make-pipe #true #false #false #true) (make-pipe #false #false #true #true))
                                 100
                                 30
                                 (make-pipe #false #true #false #false)
                                 (make-goo-flow #false #true #false #false (list (make-posn 0 0)))))
              (make-game-state
               (make-grid 3 (list (make-placed-pipe (make-pipe #false #true #false #false) 0 0 #false)))
               (list (make-pipe #true #false #false #true) (make-pipe #false #false #true #true))
               100
               30
               (make-pipe #false #true #false #false)
               (make-goo-flow #false #true #false #false (list (make-posn 0 0)))))
(check-expect (propogate-finish (make-game-state
                                 (make-grid 5 (list (make-placed-pipe (make-pipe #false #false #true #false) 0 0 #false)))
                                 '()
                                 100
                                 30
                                 (make-pipe #false #false #true #false)
                                 (make-goo-flow #false #false #true #false (list (make-posn 0 0)))))
              (make-game-state
               (make-grid 5 (list (make-placed-pipe (make-pipe #false #false #true #false) 0 0 #false)))
               '()
               100
               30
               (make-pipe #false #false #true #false)
               (make-goo-flow #false #false #true #false (list (make-posn 0 0)))))
(check-expect (propogate-finish (make-game-state
                                 (make-grid 2 (list (make-placed-pipe (make-pipe #false #true #false #false) 0 0 #false)))
                                 '()
                                 100
                                 30
                                 (make-pipe #false #true #false #false)
                                 (make-goo-flow #false #true #false #false (list (make-posn 0 0)))))
              (make-game-state
               (make-grid 2 (list (make-placed-pipe (make-pipe #false #true #false #false) 0 0 #false)))
               '()
               100
               30
               (make-pipe #false #true #false #false)
               (make-goo-flow #false #true #false #false (list (make-posn 0 0)))))
                               

;; draw-game: GameState -> Image
;; Produces the image of the grid with its placed pipes
(define (draw-game gs)
  (grid->image (game-state-grid gs) (game-state-tile-side-length gs) (game-state-pipe-width gs) (game-state-pipes-to-place gs)))

;; pipe-fantasy: GameState -> GameState
;; Starts the pipe fantasy game 
(define (pipe-fantasy initial-game-state)
  (big-bang initial-game-state
    [to-draw draw-game]
    [on-mouse place-pipe-on-click]))

;; Starts game with a bottom starting pipe at (0,0) in a 7x7 grid that uses a list of pipes
(define STATE-FOUR (gamestate-init 7 0 0 BOTTOM (list PIPE-TB PIPE-TB PIPE-TR PIPE-LR PIPE-CROSS PIPE-BL PIPE-TB PIPE-TR PIPE-TL) 100 30))
;(pipe-fantasy STATE-FOUR)

;; Starts a game with a right starting pipe at (0,1) in a 5x5 grid that uses a list of PIPES
(define STATE-FIVE (gamestate-init 5 0 1 RIGHT (list PIPE-LR PIPE-LR PIPE-LR PIPE-BL) 100 30))
;(pipe-fantasy STATE-FIVE)

