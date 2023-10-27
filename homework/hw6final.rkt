#lang racket

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw6final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define ALL-PIPES (list PIPE-TL PIPE-TR PIPE-BL PIPE-BR PIPE-TB PIPE-LR PIPE-CROSS))

(define (pipe-template p)
  (...(pipe-top p)...(pipe-bot p)...(pipe-left p)...(pipe-right p))) 


;; pipe->image: Pipe Integer Integer -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length
(define (pipe->image pipe tile-side-length pipe-width)
  (local [ ;; PS - gray frame of pipe, P-VERTICAL - vertical black component, P-HORIZONTAL - horizontal black component
    (define PS (square tile-side-length "solid" "gray"))
    (define P-VERTICAL (rectangle pipe-width (* 1/2 tile-side-length) "solid" "black"))
    (define P-HORIZONTAL (rectangle (+ (* 1/2 pipe-width) (* 1/2 tile-side-length)) pipe-width "solid" "black"))
    ]
    (cond
      ;; cross because all conds match
      [(and (pipe-top pipe) (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe))
       (overlay/align "middle" "center" (rectangle pipe-width tile-side-length "solid" "black")
       (overlay/align "middle" "center" (rectangle tile-side-length pipe-width "solid" "black") PS))]
      
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
                                             (rectangle pipe-width tile-side-length "solid" "black") PS)]
      ;; left right (across straight)
      [(and (pipe-left pipe) (pipe-right pipe))(overlay/align "middle" "center"
                                             (rectangle tile-side-length pipe-width "solid" "black") PS)])))


(define-struct placed-pipe [pipe row col]) 
;; A Placed-Pipe is a (make-placed-pipe Pipe Number Number)
;; Represents a placed pipe on a Grid where:
;; - Pipe is the type of pipe being placed
;; - row is the row number of the placed pipe
;; - column is the column number of the placed pipe

(define (placed-pipe-template p)
  (...(placed-pipe-pipe p)...(placed-pipe-row p)...(placed-pipe-col p)...))

;; EXAMPLES
(define P1 (make-placed-pipe PIPE-TL 1 2))
(define P2 (make-placed-pipe PIPE-TB 3 1))
(define P3 (make-placed-pipe PIPE-CROSS 2 2))

(define-struct grid [dimension pipes])
;; A Grid is a (make-grid number [List of Placed-Pipe])
;; Represents a grid of pipes where:
;; dimension is the number of rows and columns for the n x n grid
;; pipes is the list of placed pipes in the grid

(define (grid-template g)
  (...(grid-dimension grid)...(grid-pipes g)...))


;; Task 5 
(define STARTING-GRID (make-grid 7 '()))

;; More examples
(define GRID-1 (make-grid 6 (list (make-placed-pipe PIPE-TL 1 2))))
(define GRID-2 (make-grid 3 (list (make-placed-pipe PIPE-TL 1 1))))
(define GRID-3 (make-grid 9 (list (make-placed-pipe PIPE-TL 3 3))))


;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.
(define (place-pipe grid pipe row col)
  (make-grid (grid-dimension grid) ( cons (make-placed-pipe pipe row col) (grid-pipes grid))))

;; check expects:
(check-expect (place-pipe STARTING-GRID PIPE-TL 1 1) (make-grid 7 (list (make-placed-pipe PIPE-TL 1 1))))

(check-expect (place-pipe GRID-1 PIPE-TL 1 2) (make-grid 6 (list (make-placed-pipe PIPE-TL 1 2) (make-placed-pipe PIPE-TL 1 2))))

(check-expect (place-pipe GRID-2 PIPE-TR 2 4) (make-grid 3 (list (make-placed-pipe PIPE-TR 2 4) (make-placed-pipe PIPE-TL 1 1))))

;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.
(define (pipe-at grid row col)
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
(define (grid->image grid tile-side-length pipe-width)
  (local[
         (define ROW (row-tile tile-side-length (grid-dimension grid)))
         (define GRID (grid-from-row ROW (grid-dimension grid)))
         ]

    (cond
      [(empty? (grid-pipes grid)) GRID]
      [else (overlay/align/offset "left" "top"
            (pipe->image (placed-pipe-pipe (first (grid-pipes grid))) tile-side-length pipe-width)
            (- 0 (* tile-side-length (placed-pipe-col (first (grid-pipes grid)))))
            (- 0 (* tile-side-length (placed-pipe-row (first (grid-pipes grid)))))
            (grid->image (make-grid (grid-dimension grid) (rest (grid-pipes grid))) tile-side-length pipe-width))])))

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


(define-struct game-state [grid pipes-to-place tile-side-length pipe-width])
;; A GameState is a (make-game-state grid [List of Pipes] Number)
;; Represents the current GameState where
;; - grid is the grid to place pipes on
;; - pipes-to-place is a list of available pipes to be placed into the grid
;; - tile-side-length is the length of the tiles

;; EXAMPLES:
;; STATE-ONE is a game state with empty starting grid with some pipes to place 
(define STATE-ONE (make-game-state STARTING-GRID (list PIPE-TL PIPE-CROSS PIPE-BL PIPE-TL PIPE-CROSS PIPE-TR) 100 30))

;; STATE-TWO is a game state with an already placed pipe with some more pipes to place
(define STATE-TWO (make-game-state GRID-1 (list PIPE-CROSS PIPE-CROSS PIPE-CROSS PIPE-BL PIPE-TL) 150 50))

;; STATE-THREE is a game state with an already placed pipe with no pipes to place
(define STATE-THREE (make-game-state GRID-3 '() 150 50))

;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.
(define (place-pipe-on-click gs x y me)
  (local[
         (define SCALING-FACTOR (game-state-tile-side-length gs))
         ]
    (cond
      [(empty? (game-state-pipes-to-place gs)) gs]
      [(mouse=? me "button-down")
       (make-game-state
        (place-pipe (game-state-grid gs)
                    (first (game-state-pipes-to-place gs))
                    (floor (/ y SCALING-FACTOR)) (floor (/ x SCALING-FACTOR)))
        (rest (game-state-pipes-to-place gs))(game-state-tile-side-length gs) (game-state-pipe-width gs))]
      [else gs]))) 


;; draw-game: GameState -> Image
;; Produces the image of the grid with its placed pipes
(define (draw-game gs)
  (grid->image (game-state-grid gs) (game-state-tile-side-length gs) (game-state-pipe-width gs)))

;; pipe-fantasy: GameState -> GameState
;; Starts the pipe fantasy game 
(define (pipe-fantasy initial-game-state)
  (big-bang initial-game-state
      [to-draw draw-game]
      [on-mouse place-pipe-on-click]))

;; Starts game with state one where no pipes are pre-placed and there are pipes to place
;;(pipe-fantasy STATE-ONE)







