#lang racket

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;! Instructions:
;;! 1. Read the contents of this file, and fill in [TODO] items that appear
;;!    below.
;;! 2. Do not create, modify or delete any line that begins with ";;!!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.


(require 2htdp/image)
(require 2htdp/universe)

;;! Problem 1

;;! Consider the following data definitions & interpretations:
(define-struct address [num st city us-state zip])

;;! An Address is a (make-address Nat String String String Nat)
;;! - where num is the number of the building on the street
;;! - st is the name of the street
;;! - city is the city the building is in
;;! - us-state is the state the city is in
;;! - and zip is the zipcode of the building
;;! Interpretation: a US address

(define ADDRESS-ONE (make-address 6254 "Imaginary St" "Fake City" "NY" 11455))
(define ADDRESS-TWO (make-address 6255 "Imaginary St" "Fake City 2" "NY" 11455))
(define ADDRESS-THREE (make-address 6266 "Imaginary St" "Fake City 3" "NY" 11455))

(define (address-template Add)
  (...(address-num Add)...(address-st Add)...(address-city Add)
      ...(address-us-state Add)...(address-zip Add)...) 
  )

(define-struct student [first last nuid local perm])

;;! An NUStudent is a (make-student String String PositiveNumber Address Address)
;;! - where first is the student's first name
;;! - last is the student's last name
;;! - nuid is the student's NUID #
;;! - local is the student's local address
;;! - and perm is the student's permanent address
;;! Interpretation: a Northeastern student

(define STUDENT-ONE (make-student "Richard" "Chen" 000112090 ADDRESS-ONE ADDRESS-TWO)) 
(define STUDENT-TWO (make-student "Brian" "Dong" 000135190 ADDRESS-TWO ADDRESS-ONE)) 
(define STUDENT-THREE (make-student "Nile" "Tan" 000189012 ADDRESS-THREE ADDRESS-TWO)) 

(define (student-template student)
  (...(student-first student)...(student-last student)...
      (student-nuid student)...(student-local student)...
      (student-perm student)))

;;! Part A

;;! TODO:  Complete the data design recipe for both of the data definations above
;;! (i.e., provide examples and templates for address and student)


;;! Part B

;;! TODO: Design the function student-email which takes an NUStudent and
;;! produces a string representing that student’s email address. For simplicity
;;! we will say that a student’s email address is always their last name
;;! (all lowercase),  followed by a period, followed by the first initial
;;! of their first name (also lowercase), and finished
;;! with "@northeastern.edu".


;; student-email: NUStudent -> String
;; Takes in a NUstudent email and returns the email address
(define (student-email student) (string-downcase(string-append
                                 (student-last student) "." (student-first student) 
                                 "@northeastern.edu")))

(check-expect (student-email STUDENT-ONE) "chen.richard@northeastern.edu")
(check-expect (student-email STUDENT-TWO) "dong.brian@northeastern.edu")
(check-expect (student-email STUDENT-THREE) "tan.nile@northeastern.edu")

;;! Part C

;;! TODO: Design the function update-zipcode/address that takes an Adress and a
;;! number representing the zip code and produces a new address with the new zip code

;; update-zipcode/address: Address Num -> Address
;; Changes the zipcode of an address
(define (update-zipcode/address add new-zip)
  (make-address (address-num add) (address-st add)
                (address-city add) (address-us-state add) new-zip )
  )

(check-expect(update-zipcode/address ADDRESS-ONE 11135)
             (make-address 6254 "Imaginary St" "Fake City" "NY" 11135))

(check-expect(update-zipcode/address ADDRESS-TWO 11111)
             (make-address 6255 "Imaginary St" "Fake City 2" "NY" 11111))

(check-expect(update-zipcode/address ADDRESS-THREE 13262)
             (make-address 6266 "Imaginary St" "Fake City 3" "NY" 13262))


;;! Part D

;;! TODO: Design the function update-zipcode which takes an NUStudent and a
;;! number, representing the new zip code of the person and updates their permanent
;;! address to have that zip code. Be sure to follow the template!

;; update-zipcode: NUStudent Number
;; Changes the permanent address's zipcode of the given student 
(define (update-zipcode student num)

  (make-student(student-first student) (student-last student)
               (student-nuid student) (student-local student)
               (update-zipcode/address (student-perm student) num)))

(check-expect(update-zipcode STUDENT-ONE 11145)
             (make-student "Richard" "Chen" 000112090 ADDRESS-ONE
                           (make-address 6255 "Imaginary St" "Fake City 2" "NY" 11145)))

(check-expect(update-zipcode STUDENT-TWO 11333)
             (make-student "Brian" "Dong" 000135190 ADDRESS-TWO
                           (make-address 6254 "Imaginary St" "Fake City" "NY" 11333)))

(check-expect(update-zipcode STUDENT-THREE 11001)
             (make-student "Nile" "Tan" 000189012 ADDRESS-THREE
                           (make-address 6255 "Imaginary St" "Fake City 2" "NY" 11001)))



;;! Problem 2

;;! You are to design a program text-mover to display and manipulate text on a
;;! background. Your program should accept some phrase to show, as well as initial
;;! location and color (we only support three: red, black, or purple) - you should
;;! then display the phrase on the screen as described.

;;! When the user presses a mouse button, the program should move the text to the
;;! location that they clicked. When the user presses a key on the keyboard, the
;;! program should rotate colors.

;;! You should at least make it through Part D,where you design the text-mover
;;! function.

;;! The following is already defined in Racket:
;;! (define-struct posn [x y])
;;! A Position is a (make-posn Real Real)
;;! Interpretation: a 2D location

;;! Part A

;;! TODO: Complete the data design recipe for Position
(define (Position-template p)
  (...(posn-x p)...(posn-y)))


(define POS-1 (make-posn 0 1))
(define POS-2 (make-posn 0 0))
(define POS-3 (make-posn 2 2))



;;! Part B

;;! A RedBlackPurple (RBP) is one of:
;;! - "red"
;;! - "black"
;;! - "purple"
;;! Interpretation: available font colors

;;! TODO: Complete the data design recipe for RedBlackPurple,
;;! Use rbp-temp as the name of the template

(define RBP-RED "red")
(define RBP-BLACK "black")
(define RBP-PURPLE "purple")

(define (RBP-template RBP)
  (cond
    [(string=? RBP-RED)...]
    [(string=? RBP-BLACK)...]
    [(string=? RBP-PURPLE)...]
    ))

;;! Part C

(define-struct tm [str pos col])
;; A TextMover (TM) is a (make-tm String Position RBP)
;; - str is the text to be displayed
;; - pos is the location of the text
;; - col is the color of the text
;; Interpretation: all the information needed for the text-mover program.

;;! TODO: Complete the data design recipe for TextMover

(define TM-1 (make-tm "text" (make-posn 0 0) RBP-RED))
(define TM-2 (make-tm "foo" (make-posn 1 1) RBP-BLACK))
(define TM-3 (make-tm "bar" (make-posn 2 2) RBP-PURPLE))

(define (TM-template TM)
  (...(tm-str TM)...(tm-pos TM)...(tm-col)...)
  )

;;! Part D

;;! TODO: Design the text-mover function think through the arguments to the
;;! function, how you will represent the world state, and what handlers you need
;;! to support. Actually designing the handlers will come in subsequent parts.

;; text-mover: TextMover-> Scene
(define (text-mover tm)
  (big-bang tm
    [to-draw hnd-draw-text]
    [on-mouse hnd-mouse]
    [on-key hnd-key]
    ))


;;! Part E

;;! TODO: Design a function to serve as your to-draw handler, utilizing the templates
;;! from the previous sections.


;; hnd-draw-text: TextMover -> Image
;; Draws a screen for the text to appear on
(define (hnd-draw-text tm)
  (overlay/xy (text(tm-str tm) 12 (tm-col tm)) (posn-x (tm-pos tm)) (posn-y (tm-pos tm))
              (rectangle 200 200 "solid" "white")))

;;! Part F

;;! TODO: Design your remaining handler(s), again following the appropriate
;;! template(s).
;;! - Hint #1: for the mouse, you'll want to respond only to the "button-up"
;;!            event, which you can check using the mouse=? function.
;;! - Hint #2: make sure to follow your templates, which may involve breaking
;;!            the handlers  into helper functions.

;; hnd-mouse: TextMover Number Number String -> TextMover
;; returns a new TM indicatingt the location of the position clicked by the mouse 
(define (hnd-mouse tm x y mouse-event)
  (if (mouse=? mouse-event "button-up") (make-tm (tm-str tm) (make-posn (- x) (- y)) (tm-col tm) ) tm))

;; hnd-key: TextMover Key -> TextMover
;; Changes the color of the RBP state when space is pressed
(define (hnd-key tm key)
  (cond
    [(string=? key " ") (make-tm (tm-str tm) (tm-pos tm) (color-change (tm-col tm)))]
    [else tm]
))

;; color-change: RBP -> RBP
;; rotates the color of the given RBP
(define (color-change rbp)
  (cond
    [(string=? rbp RBP-RED) RBP-BLACK]
    [(string=? rbp RBP-BLACK) RBP-PURPLE]
    [(string=? rbp RBP-PURPLE) RBP-RED]
    )
  )

(check-expect(color-change RBP-RED) RBP-BLACK)
(check-expect(color-change RBP-BLACK) RBP-PURPLE)
(check-expect(color-change RBP-PURPLE) RBP-RED)


