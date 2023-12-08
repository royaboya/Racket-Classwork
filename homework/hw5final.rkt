#lang racket

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw5final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Instructions
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the _design recipe_ for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2023F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get on errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether or Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.

;;! Problem 1

;; Consider the three functions below (we have deliberately omitted tests and purpose
;; statements):

;; flip: [List-of Boolean] -> [List-of Boolean]
(define (flip lob)
  (cond
    [(empty? lob) '()]
    [(cons? lob) (cons (not (first lob)) (flip (rest lob)))]))


;; until-zero: [List-of Number] -> [List-of Number]
(define (until-zero lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (if (= (first lon) 0)
         '()
         (cons (first lon) (until-zero (rest lon))))]))

;; words-until-period: [List-of String] -> [List-of String]
(define (words-until-period los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (if (string=? (first los) ".")
         '()
         (cons (first los) (words-until-period (rest los))))]))

;;! Part A

;; It is possible to design a list abstraction that can be used to simplify two
;; of the three functions defined above. Design that list abstraction.
(define (until-n lop func n)
  (cond
    [(empty? lop) '()]
    [(cons? lop)
     (if (func (first lop) n)
         '()
         (cons (first lop) (until-n (rest lop) func n)))]))

;;! Part B

;; Use the list abstraction you designed in Part A to rewrite the functions
;; above that you can. Do not modify the code above. Instead, write your
;; functions here and call them flip/v2, until-zero/v2, or words-until-period/v2.

;; Testing, clean up later


;; until-zero/v2: List -> List
;; A redefined version of until-zero
(define (until-zero/v2 lop) (until-n lop = 0))

;; TEST CASES
(check-expect (until-zero/v2 (list 1 2 3 4 5 6 0 1)) (list 1 2 3 4 5 6))
(check-expect (until-zero/v2 '())'())
(check-expect (until-zero/v2 (list 0)) '())


;; words-until-period/v2: List -> List
;; A redefined version of words-until-period/v2
(define (words-until-period/v2 lop) (until-n lop string=? "."))

;; TEST CASES
(check-expect (words-until-period/v2 (list "a" "b" "c" ".")) (list "a" "b" "c")) 
(check-expect (words-until-period/v2 '()) '())
(check-expect (words-until-period/v2 (list ".")) '())

;;! Problem 2

;; The objective in this problem is to define the following functions.
;; We have given their signatures, purpose statements, and check-expects.

(define-struct pair [first second])
;; A [Pair X] is a (make-pair X X) representing a pair of any type
;; - first is the first item in the pair
;; - second is the second item in the pair


;; strings-or-odds : [List-of [Pair Number]] -> [List-of [Pair String]]
;; For each pair converts the first item to a string and the second to "odd".
(define (strings-or-odds p) (or-abstract p number->string edit-odd))

(check-expect (strings-or-odds (list (make-pair 53 23) (make-pair 40 11)))
              (list (make-pair "53" "odd") (make-pair "40" "odd")))
(check-expect (strings-or-odds (list (make-pair 20 30) (make-pair 0 1) (make-pair 3 4)))
              (list (make-pair "20" "odd") (make-pair "0" "odd") (make-pair "3" "odd")))
(check-expect (strings-or-odds '()) '())

;; alternate-case : [List-of [Pair String]] -> [List-of [Pair String]]
;; Uppercase the first item of each pair.
(define (alternate-case p) (or-abstract p string-upcase keep))


(check-expect (alternate-case (list (make-pair "hello" "world") (make-pair "this" "is")))
              (list (make-pair "HELLO" "world") (make-pair "THIS" "is")))
(check-expect (alternate-case (list (make-pair "one" "two") (make-pair "three" "four") (make-pair "five" "six")))
              (list (make-pair "ONE" "two") (make-pair "THREE" "four") (make-pair "FIVE" "six")))
(check-expect (alternate-case (list (make-pair "apple" "banana"))) (list (make-pair "APPLE" "banana")))


;; flip-or-keep-boolean : [List-of [Pair Boolean]] -> [List-of [Pair Boolean]]
;; Flip the first item of each pair, keep the second.
(define (flip-or-keep-boolean p) (or-abstract p not keep))

(check-expect (flip-or-keep-boolean (list (make-pair #true #true) (make-pair #true #true)))
              (list (make-pair #false #true) (make-pair #false #true)))
(check-expect (flip-or-keep-boolean (list (make-pair #false #false) (make-pair #false #false)))
              (list (make-pair #true #false) (make-pair #true #false)))
(check-expect (flip-or-keep-boolean (list (make-pair #true #false) (make-pair #false #true)))
              (list (make-pair #false #false) (make-pair #true #true)))

;; However, you must not _directly_ use the list template when you define them!
;;
;; Instead, first design a list abstraction (following the list template), then
;; use that abstraction to design the three functions.
(define (or-abstract p func1 func2)
  (cond
    [(empty? p) '()]
    [else (cons (make-pair (func1 (pair-first (first p))) (func2 (pair-second (first p))))
                (or-abstract (rest p) func1 func2))]))

;; keep: X -> X
;; Takes any data type X and returns it as the output
(define (keep s) s)

(check-expect(keep "s" ) "s")
(check-expect(keep #true ) #true)
(check-expect(keep 2 ) 2)

;; edit-odd: String -> String
;; Returns a string called "odd" as a helper function for strings-or-odds
(define (edit-odd s) "odd") 
(check-expect(edit-odd "no matter what" ) "odd")
(check-expect(edit-odd "odd will always" ) "odd")
(check-expect(edit-odd "be returned" ) "odd")

;;! Problem 3

;; Objective: Build a Word Game

;; Your goal is to author a word-building game. You will start with an empty 5x1 grid
;; and a hidden list of random letters. When the player clicks on a cell, its
;; contents should be replaced by the next letter in the list. The game concludes
;; when the cells spell a five-letter word. (You should build a short list of
;; five letter words.)
;;
;; Here is a video that demonstrates the game:
;;
;;   https://pages.github.khoury.northeastern.edu/2500/2023F/starter/hw5.gif
;;
;; Here are questions to help you think through your program design:
;;
;; 1. What do you need zin your world state? (What changes during the game?)
;;    Come up with a data design to represent the world state.
;;
;; 2. Your program needs to draw a board, handle mouse clicks, and stop when
;;    the player constructs a word or runs out of letters. These are three 
;;    functions that you need to design.
;;
;; 3. Finally, put it all together using big-bang.

(require 2htdp/image)
(require 2htdp/universe)

(define WORD-LIST (list "hands" "whole" "straw"))
(define LETTERS (list " " "a" "d" "e" "h" "l" "n" "o" "r" "s" "t" "w"))
(define INIT-STATE (list "     " "a"))

(define (word-build word-letter)
  (big-bang word-letter
    [to-draw hnd-draw]
    [on-mouse hnd-mouse]
    [stop-when hnd-end hnd-draw]))

;; hnd-draw : [ListOf String] -> Image
;; Draws a 5x1 board with the given word split by letter
(define (hnd-draw word-letter)
  (beside
   (overlay
    (text (substring (first word-letter) 0 1) 24 "black")
    (square 50 "outline" "black"))
   (overlay
    (text (substring (first word-letter) 1 2) 24 "black")
    (square 50 "outline" "black"))
   (overlay
    (text (substring (first word-letter) 2 3) 24 "black")
    (square 50 "outline" "black"))
   (overlay
    (text (substring (first word-letter) 3 4) 24 "black")
    (square 50 "outline" "black"))
   (overlay
    (text (substring (first word-letter) 4 5) 24 "black")
    (square 50 "outline" "black"))))

(check-expect (hnd-draw (list "hands" "t"))
              (beside
               (overlay
                (text "h" 24 "black")
                (square 50 "outline" "black"))
               (overlay
                (text "a" 24 "black")
                (square 50 "outline" "black"))
               (overlay
                (text "n" 24 "black")
                (square 50 "outline" "black"))
               (overlay
                (text "d" 24 "black")
                (square 50 "outline" "black"))
               (overlay
                (text "s" 24 "black")
                (square 50 "outline" "black"))))
(check-expect (hnd-draw (list "whol " "r"))
              (beside
               (overlay
                (text "w" 24 "black")
                (square 50 "outline" "black"))
               (overlay
                (text "h" 24 "black")
                (square 50 "outline" "black"))
               (overlay
                (text "o" 24 "black")
                (square 50 "outline" "black"))
               (overlay
                (text "l" 24 "black")
                (square 50 "outline" "black"))
               (overlay
                (text " " 24 "black")
                (square 50 "outline" "black"))))
(check-expect (hnd-draw (list "     " "a"))
              (beside
               (overlay
                (text " " 24 "black")
                (square 50 "outline" "black"))
               (overlay
                (text " " 24 "black")
                (square 50 "outline" "black"))
               (overlay
                (text " " 24 "black")
                (square 50 "outline" "black"))
               (overlay
                (text " " 24 "black")
                (square 50 "outline" "black"))
               (overlay
                (text " " 24 "black")
                (square 50 "outline" "black"))))

;; get-next-letter : [ListOf String] String -> String
;; Takes a letter and returns the next letter after it within letters, the hidden list of letters
(define (get-next-letter list ltr)
  (cond
    [(null? list) (first LETTERS)] 
    [(string=? (first list) ltr)
     (if (null? (rest list)) (first LETTERS) (second list))]
    [else (get-next-letter (rest list) ltr)]))

(check-expect (get-next-letter LETTERS "a") "d")
(check-expect (get-next-letter LETTERS " ") "a")
(check-expect (get-next-letter LETTERS "w") " ")
(check-expect (get-next-letter '() "w") " ")

;; hnd-mouse : [ListOf String] Number Number String -> [ListOf String]
;; Updates the letter in the box clicked by the user to the next letter in letters, the hidden list of letters
(define (hnd-mouse word-letter x y event)
  (cond
    [(and (<= 0 x 50) (string=? event "button-up"))
     (list (string-append (first (rest word-letter)) (substring (first word-letter) 1 5)) (get-next-letter LETTERS (first (rest word-letter))))]
    [(and (<= 50 x 100) (string=? event "button-up")) 
     (list (string-append (substring (first word-letter) 0 1) (first (rest word-letter)) (substring (first word-letter) 2 5)) (get-next-letter LETTERS (first (rest word-letter))))]
    [(and (<= 100 x 150) (string=? event "button-up")) 
     (list (string-append (substring (first word-letter) 0 2) (first (rest word-letter)) (substring (first word-letter) 3 5)) (get-next-letter LETTERS (first (rest word-letter))))]
    [(and (<= 150 x 200) (string=? event "button-up")) 
     (list (string-append (substring (first word-letter) 0 3) (first (rest word-letter)) (substring (first word-letter) 4 5)) (get-next-letter LETTERS (first (rest word-letter))))]
    [(and (<= 200 x 250) (string=? event "button-up")) 
     (list (string-append (substring (first word-letter) 0 4) (first(rest word-letter))) (get-next-letter LETTERS (first (rest word-letter))))]
    [else word-letter]))

(check-expect (hnd-mouse (list "     " "a") 10 10 "button-up") (list "a    " "d"))
(check-expect (hnd-mouse (list "     " "a") 60 10 "button-up") (list " a   " "d"))
(check-expect (hnd-mouse (list "     " "a") 110 10 "button-up") (list "  a  " "d"))
(check-expect (hnd-mouse (list "     " "a") 160 10 "button-up") (list "   a " "d"))
(check-expect (hnd-mouse (list "     " "a") 210 10 "button-up") (list "    a" "d"))
(check-expect (hnd-mouse (list "     " "a") 210 10 "button-down") (list "     " "a"))

;; in-list? : String [ListOf String] -> Boolean
;; Returns #true if and only if the given string is found within the list
(define (in-list? str list)
  (cond
    [(empty? list) #f]
    [(string=? str (first list)) #t] 
    [else (in-list? str (rest list))]))

(check-expect (in-list? "hands" WORD-LIST) #t)
(check-expect (in-list? "finger" WORD-LIST) #f)
(check-expect (in-list? "straw" WORD-LIST) #t)
  
;; hnd-end : [ListOf String] -> Boolean
;; Purpose: Returns #true if the given word is in the word-list, the list of final words
(define (hnd-end word-letter)
    (in-list? (first word-letter) WORD-LIST))

(check-expect (hnd-end (list "hands" "t")) #t)
(check-expect (hnd-end (list "finger" " ")) #f)
(check-expect (hnd-end (list "straw" "o")) #t)

;; (word-build INIT-STATE)