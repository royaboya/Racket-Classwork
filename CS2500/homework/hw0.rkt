;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#lang racket
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |hw0 (final version)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;! Purpose: An introduction to programming with simple function definitions.

;;! Instructions:
;;! 1. Read the contents of this file, and fill in [TODO] items that appear
;;!    below.
;;! 2. Do not create, modify or delete any line that begins with ";;!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.


(require 2htdp/image)

;;! Problem 1

;;! Part A

;; Examine the following function, and:
;; i.   Write down its signature,
;; ii.  Give it an informative purpose statement, and
;; iii. Give the function and its arguments more informative names.

;; [TODO] Signature
;; [TODO] Purpose

;; detective: Number, Number, String -> String
;; Determines if given person is eligible to ride an amusement park ride
;; given the condition that they are greater or less than 48 inches.
;; Riders greater than 48 inches are not permitted to ride. 


(define (detective a b c)
  (if (< (+ (* 12 a) b) 48)
      (string-append "Welcome aboard, " c "!")
      "Sorry, you may not board the ride. :("))

;; [TODO] Better names for detective and its arguments
;; Better function name: amusement-park-ride
;; Argument Suggestions:
;; a - feet
;; b - inches
;; c - name


;;! Part B

;; Write the signature of the following function:

;; [TODO] Signature

;; mystery: string, string, number -> string
(define (mystery x y z)
  (string-append (substring x 0 z)
                 (substring y z)
                 (substring y 0 z)
                 (substring x z)))


;;! Part C

;; Describe the values that mystery produces when you apply it to two identical
;; arguments for x and y.

;; [TODO] Prose description as a comment.

;; the function mystery creates a string with its repeat appended right after.  

;;! Problem 2

;; You and your friend have been arguing about how best to invest some money.
;; You think you've picked some stocks that give decent gains consistently, but
;; your friend really wants to invest in cryptocurrencies, which she believes
;; have substantially larger gains some good days, but also suffer some losses
;; on some bad days. To settle the debate of which to invest in, you offer to
;; program a simulation of the two choices.

;;! Part A

;; Define a function stock-day which simulates a single day of gain from the
;; stocks you have in mind. Specifically, stock-day should receive as its
;; argument the amount of money you have, and should produce the amount you
;; will have after a 4% gain. For example, (stock-day 1000) should produce 1040.
;; In addition, write three examples. Here is one to get you started:
;;
;; (stock-day 1000) ; produces 1040

;; [TODO] Function definition
;; [TODO] Examples

(define (stock-day current-money)(* current-money 1.04))

;;(stock-day 300) ; produces 312
;;(stock-day 1090) ; produces 1133.6
;;(stock-day 16000) ;produces 16640
;;! Part B

;; Define a function crypto-good-day which simulates a single day of gain from
;; cryptocurrencies, assuming it was a good day. Specifically, crypto-good-day
;; should calculate the amount of money you will have after a 10% gain.
;; You must also write three examples of this function.

;; [TODO] Function definition
;; [TODO] Three examples

(define (crypto-good-day current-money)
  (* current-money 1.1))

;;(crypto-good-day 10090) ; produces 11099
;;(crypto-good-day 4000) ; produces 4400
;;(crypto-good-day 480) ; produces 528
;;! Part C

;; Define a function crypto-bad-day which simulates a single day of loss from
;; cryptocurrencies, assuming it was a bad day. Specifically, crypto-bad-day
;; should compute the total amount of money you will have after a –2% loss.
;; For example, if you start with $100 in crypto, after a bad day, you will
;; have $98 left. You must also write three examples for this function.

;; [TODO] Function definition
;; [TODO] Three examples

(define(crypto-bad-day current-money) (* current-money 0.98))

;;(crypto-bad-day 105) ; produces 102.9
;;(crypto-bad-day 1600) ; produces 1568
;;(crypto-bad-day 10000) ; produces 9800

;;! Part D

;; Define a constant STOCK-6-DAYS which is the total amount of money you would
;; have after starting with $1000 and repeatedly investing all of it in stocks
;; six days. You must use the stock-day function.
;; Hint: You can use the value produced by stock-day on the first day as the
;; argument for stock-day on the second day, and so on.

;; [TODO] Define the constant
(define STOCK-6-DAYS (stock-day(stock-day(stock-day(stock-day(stock-day(stock-day 1000)))))))


;; Define a constant CRYPTO-6-DAYS which  simulates cryptocurrency trading for
;; 6 days starting with $1,000, alternating crypto-good-day and crypto-bad-day,
;; **starting with crypto-good-day**.

;; [TODO] Define the constant
(define CRYPTO-6-DAYS(crypto-bad-day(crypto-good-day(crypto-bad-day(crypto-good-day(crypto-bad-day(crypto-good-day 1000)))))))

;;! Part E

;; Now compare the results! Which one seems to have done better?

;; [TODO] Write which one seems better? Write it as a comment here.

;; STOCK-6-DAYS seems to have done better as the result of 1265.319018496 is greater than
;; 1252.726552 after running the constants in the console.


;; <file "hw1-problem3.rkt">

;;! Problem 3

;; In western classical music, tones are typically placed on a scale called
;; the twelve-tone scale. We use non-negative integers to refer to each tone.
;; For example 60 refers to the tone called "C" (or the "do" in "do-re-mi")
;; near the middle of a piano, whereas 61 refers to the tone one unit higher.
;; We consider two tones with a gap of a multiple of 12 units between them as
;; equivalent. For example, the tones 0, 60 and 84 are all equivalent: they are
;; all the tone "C". However, tones 60 and 67 are not equivalent.

;;! Part A

;; Define a function called tone-class which consumes a single tone as an
;; argument, and produces its *class*,  which is the smallest non-negative
;; integer that is equivalent to the tone. For example, the class of 60 is 0,
;; the class of 61 is 1, and the class of 0 is 0 itself. You must also write
;; three examples for your function.

;; Hint: Since there are 12 classes starting with zero, you can calculate the
;; class as the remainder. Try looking for relevant functions in the DrRacket
;; Help Desk.

;; [TODO] Function definition
;; [TODO] Three examples

(define (tone-class tone) (modulo tone 12))
;;(tone-class 57) ; produces 9
;;(tone-class 84) ; produces 0
;;(tone-class 14) ; produces 2

;;! Part B

;; The distance between two tones is how far apart they are, while keeping
;; equivalence in mind. Since there are 12 tone classes, the maximum distance
;; between any pair of tones is 12. However, there are two distances you can
;; produce, depending on which tone you consider first:

;; - The distance between tones 60 and 63 is either 3 (counting up) or 9
;;   (counting down).
;; - The distance between 60 and 75 is also either 3 or 9.
;; - The distance between 63 and 70 is 5 or 7.

;; Write a function called tone-distance which consumes two tones as arguments,
;; and produces their distance (either distance), as defined above.
;; Write three examples for tone-distance.

;; [TODO] Function definition
;; [TODO] Thee examples

(define (tone-distance tone-one tone-two) (abs(- (tone-class tone-one) (tone-class tone-two))))
;;(tone-distance 60 75) ; produces 3
;;(tone-distance 35 12) ; produces 11
;;(tone-distance 13 19) ; produces 6

;;! Part C

;; On a piano keyboard, each class of twelve tones (a.k.a., an octave) are
;; placed in a standard pattern of eight white and five black keys. If you are
;; not familiar with this pattern, here is a picture of a piano keyboard:

;; https://en.wikipedia.org/wiki/Musical_keyboard#/media/File:Klaviatur-3-en.svg

;; Write a function called keyboard that consumes the height and width
;; of the white keys, and produces an image that looks like a piano octave.
;; The black keys are roughly half the width and about 3/4 the length of the
;; white keys.

;; Note: The picture linked above labels the white keys. Your image does not
;; have to do so.

;; Hint 1: The overlay/align function may be very helpful.

;; Hint 2: You can use "transparent" as a color for a rectangle.

(define (keyboard width height)
  (overlay/offset(rectangle(* 0.25 width)(* 0.75 height) "solid" "black") (+ (* 0.09375 width )(* -4 width)) (* 0.125 height) (overlay/offset(rectangle(* 0.5 width) (* 0.75 height) "solid" "black") (* -2 width) (* 0.125 height) (overlay/offset(rectangle(* 0.5 width)(* 0.75 height) "solid" "black")(* -1 width)(* 0.125 height)(overlay/offset(rectangle(* 0.5 width) (* 0.75 height) "solid" "black") 0 (* 0.125 height)
  (overlay/offset(rectangle(* 0.5 width)(* 0.75 height) "solid" "black") (* 2 width) (* 0.125 height) 
  (overlay/offset(rectangle (* 0.5 width)(* 0.75 height) "solid" "black") (* 3 width)(* 0.125 height)(beside
  (rectangle width height "outline" "black") (rectangle width height "outline" "black")
  (rectangle width height "outline" "black") (rectangle width height "outline" "black")
  (rectangle width height "outline" "black") (rectangle width height "outline" "black")
  (rectangle width height "outline" "black") (rectangle width height "outline" "black"))))
  )))))


