#lang racket

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; Purpose: An introduction to data design (enumerations) and the design recipe.

;;! Instructions:
;;! 1. Read the contents of this file, and fill in [TODO] items that appear
;;!    below.
;;! 2. Do not create, modify or delete any line that begins with ";;!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.
;;! 3. You must follow the _design recipe_ for every problem. In particular,
;;!    every function you define must have at least three check-expects (and
;;!    more if needed).

;;! Problem 1

;; Design a function called concat-space-separator-when-long that consumes two
;; strings and produces a single string that concatenates them. In the result,
;; the two strings should be separated by a space *only if* the first string
;; is longer than 5 characters.

;; concat-space-separator-when-long: String String -> String
;; Purpose: Returns the first string concatenated with the second
;; string with a space in between if the first string is greater
;; than five characters, otherwise returns the two strings
;; concatenated together. 
(define (concat-space-separator-when-long str-one str-two)
  (if (>(string-length str-one)5) (string-append str-one " " str-two) (string-append str-one str-two)) 
  )

(check-expect(concat-space-separator-when-long "test" "test2") "testtest2")
(check-expect(concat-space-separator-when-long "rocket" "ship") "rocket ship")
(check-expect(concat-space-separator-when-long "dog" "house") "doghouse")

;;! Problem 2

;;! Part A

;; Our solar systems traditionally had nine planets. Look them up, and
;; write a data definition called Planet that can represent any one of them.
;; NOTE: name your template planet-template.


; A Planet is one of:
; "Mercury"
; "Venus"
; "Earth"
; "Mars"
; "Jupiter"
; "Saturn"
; "Uranus"
; "Neptune"
; "Pluto"

; Interpretation: A planet in the solar system 

(define MERCURY "Mercury")
(define VENUS "Venus")
(define EARTH "Earth")
(define MARS "Mars")
(define JUPITER "Jupiter")
(define SATURN "Saturn")
(define URANUS "Uranus")
(define NEPTUNE "Neptune")
(define PLUTO "Pluto")

(define (planet-template Planet)
  (cond
    [(string=? Planet) ...]
    [(string=? Planet) ...]
    [(string=? Planet) ...]
    [(string=? Planet) ...]
    [(string=? Planet) ...]
    )
  )
;;! Part B

;; One way to classify planets is as either terrestrial, gas giant, or dwarf planet.
;; Design a function called planet-kind that consumes a Planet and produces either
;; "terrestrial", "gas giant", or "dwarf planet".

;; planet-kind: Planet -> String
;; Returns a string describing the type of Planet inputted into the function
(define(planet-kind Planet)
  (cond
    [(or(or(or(string=? Planet MERCURY)(string=? Planet VENUS))(string=? Planet EARTH))(string=? Planet MARS))"Terrestrial"]
    [(or(or(or(string=? Planet NEPTUNE)(string=? Planet URANUS))(string=? Planet SATURN))(string=? Planet JUPITER))"Gas-giant"]
    [(string=? Planet PLUTO) "Dwarf"]
    )
  )

(check-expect(planet-kind "Mercury") "Terrestrial")
(check-expect(planet-kind "Venus") "Terrestrial")
(check-expect(planet-kind "Earth") "Terrestrial")
(check-expect(planet-kind "Mars") "Terrestrial")
(check-expect(planet-kind "Neptune") "Gas-giant")
(check-expect(planet-kind "Uranus") "Gas-giant")
(check-expect(planet-kind "Saturn") "Gas-giant")
(check-expect(planet-kind "Jupiter") "Gas-giant")
(check-expect(planet-kind "Pluto") "Dwarf")


;;! Part C

;; Design a predicate called has-moons? that produces true if a planet has any
;; moons.


;; has-moons: Planet-> Boolean
;; Returns a #true or #false boolean depending on whether or not a given Planet has Moons. 
(define (has-moons? Planet)
  (cond
    [(string=? "Mercury" Planet) #f]
    [(string=? "Venus" Planet) #f]
    [(string=? "Earth" Planet) #t]
    [(string=? "Mars" Planet) #t]
    [(string=? "Jupiter" Planet) #t]
    [(string=? "Saturn" Planet) #t]
    [(string=? "Uranus" Planet) #t]
    [(string=? "Neptune" Planet) #t]
    [(string=? "Pluto" Planet) #t]
    
    )
  )

(check-expect(has-moons? "Mercury")#f)
(check-expect(has-moons? "Venus")#f)
(check-expect(has-moons? "Earth")#t)
(check-expect(has-moons? "Mars")#t)
(check-expect(has-moons? "Jupiter")#t)
(check-expect(has-moons? "Saturn")#t)
(check-expect(has-moons? "Uranus")#t)
(check-expect(has-moons? "Neptune")#t)
(check-expect(has-moons? "Pluto")#t)

;;! Problem 3

;;! Part A

;; Design a data definition called RainbowColor that represents a color of the
;; rainbow. To avoid ambiguity, use the "modern" colors from this Wikipedia page:
;; https://en.wikipedia.org/wiki/Rainbow
;; NOTE: call your template rainbow-color-template.

;; A RainbowColor is one of:
;; "Red"
;; "Orange"
;; "Yellow"
;; "Green"
;; "Cyan"
;; "Blue"
;; "Violet"

;; Interpretation: A modern color on the rainbow

(define RED "Red")
(define ORANGE "Orange")
(define YELLOW "Yellow")
(define GREEN "Green")
(define CYAN "Cyan")
(define BLUE "Blue")
(define VIOLET "Violet")

(define (rainbow-color-template RainbowColor)
  (cond
    [(string=? RED RainbowColor)...]
    [(string=? ORANGE RainbowColor)...]
    [(string=? YELLOW RainbowColor)...]
    [(string=? GREEN RainbowColor)...]
    [(string=? CYAN RainbowColor)...]
    [(string=? BLUE RainbowColor)...]
    [(string=? VIOLET RainbowColor)...]
    )
  )


;;! Part B

;; Design a predicate called primary? to determine if a RainbowColor is a primary
;; color (red, yellow, or blue).

;; primary?: RainbowColor -> Boolean
;; Returns a true or false boolean depending on whether or not a RainbowColor
;; is a primary color
(define (primary? RainbowColor)
  (or(or(string=? RainbowColor RED)(string=? RainbowColor YELLOW))(string=? RainbowColor BLUE))
  )

(check-expect (primary? BLUE) #t)
(check-expect (primary? ORANGE) #f)
(check-expect (primary? YELLOW) #t)


;;! Part C

;; Design a function called next-color that consumes a RainbowColor and produces
;; the next color, where next goes from outside to inside of a rainbow. When
;; applies to the innermost color (violet), it produces the outermost color (red).

;; next-color: RainbowColor -> RainbowColor
;; Returns the next  RainbowColor in order from the outside to inside of the rainbow
;; where Red is the most outer RainbowColor and Violet is the most inner RainbowColor
(define (next-color RainbowColor)
  (cond
    [(string=? RED RainbowColor) ORANGE]
    [(string=? ORANGE RainbowColor) YELLOW]
    [(string=? YELLOW RainbowColor) GREEN]
    [(string=? GREEN RainbowColor) CYAN]
    [(string=? CYAN RainbowColor) BLUE]
    [(string=? BLUE RainbowColor) VIOLET]
    [(string=? VIOLET RainbowColor) RED]
    )
  )

(check-expect(next-color RED) ORANGE)
(check-expect(next-color ORANGE) YELLOW)
(check-expect(next-color YELLOW) GREEN)
(check-expect(next-color GREEN) CYAN)
(check-expect(next-color CYAN) BLUE)
(check-expect(next-color BLUE) VIOLET)
(check-expect(next-color VIOLET) RED)

