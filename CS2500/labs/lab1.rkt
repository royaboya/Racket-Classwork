;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#lang racket
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;! Instructions:
;;! 1. Read the contents of this file, and fill in [TODO] items that appear
;;!    below.
;;! 2. Do not create, modify or delete any line that begins with ";;!!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.


(require 2htdp/image)
(require 2htdp/universe)

;;! Problem 1

;; TODO: Design the function string-starts-with? which takes two
;; Strings and returns a Boolean indicating whether the first
;; string begins with the second. Be sure to follow all the steps
;; of the Design Recipe for functions.

;; When you are testing your function, make sure you test the case
;; where the first string is shorter than the second. For example
;; (string-starts-with? "fundies" "fun") should return #true but
;; (string-starts-with? "fun" "fundies") should return #false.

;; check if second string is substring of first string

;; string-starts-with?: String String -> Boolean
;; Returns a boolean on whether or not the second string is a starting substring
;; of the first string. Function is case sensitive
(define (string-starts-with? str1 str2)
  (if(> (string-length str2) (string-length str1)) #f
     (string=? str2 (substring str1 0 (string-length str2)))))

(check-expect(string-starts-with? "doghouse" "dog")#t)
(check-expect (string-starts-with? "bulldozer" "bull") #t)
(check-expect (string-starts-with? "yeeyee" "yeeyeeyee") #f)


;;! Problem 2

;; TODO: Design the function either-true? that takes two
;; Boolean parameters and returns true if either (or both)
;; of the parameters are true.

;; You must adhere to the following restrictions:
;;
;; - you are only allowed to use if, the names of the
;;   parameters, #true, and #false (though you may not
;;   need all of these);
;;
;; - you are not allowed to use an if that takes
;;   the following form (if parameter #true #false),
;;   since this is the same as the value of parameter;
;;
;; - the tests for your function should cover ALL possible
;;   input combinations for the parameters.
;;
;; And don't forget (for the rest of the class!), "designing" a function
;; means to produce all 4 parts of the Design Recipe for functions!

;; either-true?: Boolean Boolean -> Boolean
;; Returns a #true boolean if at least one of the inputs is true, otherwise returns #false
(define (either-true? bool1 bool2) (or bool1 bool2))

(check-expect(either-true? #t #f) #t)
(check-expect(either-true? #f #f) #f)
(check-expect(either-true? #t #t) #t)

;;! Problem 3

;; You are to design a small door-simulator program...
;;
;; - A door can either be open, closed, or locked. Your program
;;   will take in a representation of one of these states.
;;
;; - The user can open a closed door by pressing the "o"
;;   key on their keyboard. You cannot open a locked door,
;;   and attempting to open an already open door will do nothing.
;;
;; - The user can close an open door by pressing the "c" key
;;   on their keyboard. Attempting to close an already closed
;;   (or closed and locked) door will do nothing.
;;
;; - The user can lock a closed door by pressing the "l" key
;;   on their keyboard. Attempting to lock an open door or an
;;   already locked door will do nothing.
;;
;; - The user can unlock a locked door by pressing the "u" key
;;   on their keyboard. Attempting to unlock a closed door that
;;   is already unlocked, or an open door, will do nothing.


;;! A DoorState is one of:
;;! - "closed"
;;! - "locked"
;;! - "open"
;;! Interpretation: state of a lockable door

;;! Part A

;; TODO: finish the Design Recipe for data for DoorState
;; (so provide examples and a template called ds-temp)


(define (ds-temp DoorState)
  (cond
    [(string=? DoorState)...]
    [(string=? DoorState)...]
    [(string=? DoorState)...]))

(define CLOSED "closed")
(define LOCKED "locked")
(define OPEN "open")


(define (ds-draw ds)
  (cond
    ;;[( 0 DoorState) DOOR-CLOSED]
    [(string=? CLOSED ds) DOOR-CLOSED]
    [(string=? LOCKED ds) DOOR-LOCKED]
    [(string=? OPEN ds) DOOR-OPEN]
    
    ))

(define (hnd-key-pressed ds keyword)
  (cond
    [(and (string=? keyword "o") (not(string=? ds LOCKED))) OPEN]
    [(and(string=? keyword "c") (not(string=? ds LOCKED)))CLOSED]
    [(and(string=? keyword "u") (string=? ds LOCKED)) CLOSED] ;; locked for now idk how to change
    [(and(string=? keyword "l") (string=? ds CLOSED))LOCKED]
    [else ds]
    ))



;;! Part B

;; TODO: write a function door-simulator that calls
;; big-bang; in addition to to-draw, what handler(s) will
;; you need for the description above?
;; NOTE: Prefix all your handlers with hnd- to facilitate grading.

;; four distinct key: o,c,u,l
;; what is a handler?
;; to-draw
;; key-event


(define (door-simulator CLOSED)
  (big-bang CLOSED
    [to-draw ds-draw]
    [on-key hnd-key-pressed]
    ))
                            



;;! Part C

;; TODO: design all the handlers in the "wish list" you
;; just generated (via your big-bang event handlers). To help
;; we've provided some examples of visualizations of the states
;; of the door (you can use these or make your own).

(define BG (rectangle 400 200 "solid" "blue"))

(define DOOR-W (/ (image-width BG) 5))
(define DOOR-H (- (image-height BG) 40))

(define KNOB-X (* .8 DOOR-W))
(define KNOB-Y (/ DOOR-H 2))

(define DOOR
  (place-image
   (circle (/ DOOR-W 10) "solid" "gray")
   KNOB-X KNOB-Y
   (rectangle DOOR-W DOOR-H "solid" "brown")))

(define DOOR-LOCK
  (place-image
   (text "x" 10 "black")
   KNOB-X KNOB-Y
   DOOR))

(define DOOR-X (* 0.6 (image-width BG)))
(define DOOR-Y (+ (/ DOOR-H 2) (- (image-height BG) DOOR-H)))

(define DOOR-CLOSED
  (place-image
   DOOR
   DOOR-X DOOR-Y
   BG))

(define DOOR-LOCKED
  (place-image
   DOOR-LOCK
   DOOR-X DOOR-Y
   BG))

(define DOOR-OPEN
  (place-image
   (beside (flip-horizontal DOOR)
           (rectangle DOOR-W DOOR-H "solid" "lightblue"))
   (- DOOR-X (/ DOOR-W 2)) DOOR-Y
   BG))

;; NOTE: Your submission should not start the door-simulator when Run.
;; You should instead apply (door-simulator ...) from the interactions window.
;; You will get a "timeout" message on Gradescope (and lose points) if you submit
;; a program that starts the simulation when Run.
