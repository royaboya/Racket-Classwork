#lang racket
(require 2htdp/image)
;; Purpose: Recipe recipe practice, now with structured data.

;;! Instructions
;;! 1. Do not create, modify or delete any line that begins with ";;!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.
;;! 2. You must follow the _design recipe_ for every problem. In particular,
;;!    every function you define must have at least three check-expects (and
;;!    more if needed).
;;! 3. You must follow the Style Guide:
;;!    https://pages.github.khoury.northeastern.edu/2500/2023F/style.html
;;! 4. You must submit working code. In DrRacket, ensure you get on errors
;;!    when you click Run. After you submit on Gradescope, you'll get instant
;;!    feedback on whether or Gradescope can run your code, and your code must
;;!    run on Gradescope to receive credit from the autograder.

;;! Problem 1

;; Consider the following data definition and interpretation.

(define-struct time (hours minutes seconds))
;;! A Time is a (make-time Number Number Number)
;;! Represents the time of day where:
;;! – hours is between 0 and 23
;;! – minutes is between 0 and 59
;;! – seconds is between 0 and 59

;;! Part A
;; Complete the two remaining parts of the data design for Time.

(define noon (make-time 12 0 0))
(define midnight (make-time 0 0 0))
(define morning-hour (make-time 9 0 0))

;;; (define (time-template Time)
;;;   (cond
;;;     [(= (time-hours Time))...]
;;;     [(= (time-minutes Time))...]
;;;     [(= (time-seconds Time))...]
;;;     ))

;;! Part B
;; Design a function called tick that adds one second to a Time.

;; tick: Time -> Time
;; returns a time struct with one second elapsed from the inputted time struct
(define (tick t)
  (cond
    [(and (=(time-hours t)23) (=(time-minutes t)59) (=(time-seconds t) 59)) (make-time 0 0 0)] ;; reset case 
    [(and (=(time-minutes t)59) (=(time-seconds t) 59)) (make-time (+(time-hours t)1)0 0)] ;; hour goes up 
    [(=(time-seconds t) 59) (make-time (time-hours t) (+(time-minutes t)1) 0)] ;; minute goes up
    [else (make-time (time-hours t) (time-minutes t) (+(time-seconds t)1))] ;; second goes up by 1s
    
    ))

;;; (check-expect(tick (make-time 23 59 59)) (make-time 0 0 0))
;;; (check-expect(tick (make-time 3 59 59)) (make-time 4 0 0))
;;; (check-expect(tick (make-time 1 3 59)) (make-time 1 4 0))
;;; (check-expect(tick (make-time 1 3 3)) (make-time 1 3 4))

;;! Part C

;; Design a function called time->image that draws an analog clock face with
;; three hands. (The hour hand is shortest and the minute and second hand should
;; be different.)
;;
;; See the link below for a refresher on how an analog clock works
;; https://en.wikipedia.org/wiki/Clock_face
;; Note: The hour hand does not need to base it's position on the minute hand
;; for this assignment


(define h-hand (line 0 40 "black"))
(define m-hand (line 0 90 "black"))
(define s-hand (line 0 90 "red"))

(define (time->image time)
  (overlay/offset (rotate (- 360 (angle-MS (time-seconds time))) s-hand)
                  (* (get-x (angle-MS (time-seconds time)) 45) -1) (get-y (angle-MS (time-seconds time)) 45)
  
  (overlay/offset (rotate (- 360 (angle-MS (time-minutes time))) m-hand)
                  (* (get-x (angle-MS (time-minutes time)) 45) -1) (get-y (angle-MS (time-minutes time)) 45)

  (overlay/offset (rotate (- 360(angle-H (time-hours time))) h-hand)
                  (* (get-x (angle-H (time-hours time)) 20)-1) (get-y (angle-H (time-hours time)) 20)
                     (overlay (circle 5 "solid" "black")(circle 100 "outline" "black"))))))

(define (get-y angle magnitude)
  (* magnitude (cos (degrees->radians angle))
  ))

(define (get-x angle magnitude)
  (* magnitude (sin (degrees->radians angle))
  ))

(define (degrees->radians deg) (/ deg 57.295779))

;; angle-MS: Number -> Number
(define (angle-MS t) (modulo(* 6 t) 360))

;; angle-H: Number -> Number
(define (angle-H t) (modulo (* 30 t) 720))


; TODO: DELETE LINE
(time->image (make-time 10 30 26))

;;! Problem 2

;;! Part A

;; You are a feared restaurant critic whose ratings can make or break the
;; restaurants in Boston. Design a data definition called Review
;; that represents your review of a single restauant. A Review holds the
;; restaurant's name, its cuisine, the dish you ordered, its price, your
;; rating (1--5), and whether or not you saw any rats.

(define-struct Review (name cuisine dish price rating has-rats))
;; A Review is a (make-Review String String String Number Number Boolean)
;; name - Name of the restaurant
;; cuisine - The type of cuisine the restaurant serves
;; dish - Dish ordered
;; price - Price of dish
;; rating - Given rating to restaurant in the range of [1,5]
;; has-rats - Whether or not rats were seen at the restaurant
;; A review of a given restaurant

;;; (define (Review-template Review)
;;;   (...(Review-name Review)...(Review-name Review)...(Review-cuisine Review)
;;;       ...(Review-dish Review)...(Review-price Review)...(Review-rating Review)...(Review-has-rats Review))
;;;   )

;;! Part B

;; Design a function called update-rating that takes a Review and a new rating,
;; and updates the review with the new rating.

;; update-rating: Review Number -> Review
;; Updates a given review's rating
(define (update-rating Rev new-rating)
  (make-Review (Review-name Rev) (Review-cuisine Rev) (Review-dish Rev)
               (Review-price Rev) new-rating (Review-has-rats Rev))
  )

;;; (check-expect (update-rating (make-Review "New China" "Chinese" "Crab" 14.99 3 #f) 4) (make-Review "New China" "Chinese" "Crab" 14.99 4 #f))
;;; (check-expect (update-rating (make-Review "Chipotle" "Mexican" "Rice Bowl" 10.50 4 #f) 3) (make-Review "Chipotle" "Mexican" "Rice Bowl" 10.50 3 #f))
;;; (check-expect (update-rating (make-Review "McDonalds" "American" "Big Mac" 5.50 3 #f) 2) (make-Review "McDonalds" "American" "Big Mac" 5.50 2 #f))

;;! Part C

;; Design a function called rat-sighting that takes a Review and marks it as
;; a restaurant with rats. It also decreases its rating by 1 star, only if
;; the restaurant was not previously known to have rats.


;; rat-sighting: Review -> Review
;; Marks a restaurant with rats and decreases its rating by a star if the restaurant's previous has-rats value was false
(define (rat-sighting Rev)
  (cond
    [(Review-has-rats Rev) (make-Review (Review-name Rev) (Review-cuisine Rev) (Review-dish Rev)
                                 (Review-price Rev) (Review-rating Rev) #t )]

    [(= (Review-rating Rev) 1) (make-Review (Review-name Rev) (Review-cuisine Rev) (Review-dish Rev)
                                 (Review-price Rev) (Review-rating Rev) #t )]
    
    [else (make-Review (Review-name Rev) (Review-cuisine Rev) (Review-dish Rev)
                                 (Review-price Rev) (-(Review-rating Rev)1) #t )]
    ))

;;; (check-expect (rat-sighting (make-Review "r1" "chinese" "rice" 1.50 1 #f)) (make-Review "r1" "chinese" "rice" 1.50 1 #t))
;;; (check-expect (rat-sighting (make-Review "r2" "thai" "pho" 13 4 #f)) (make-Review "r2" "thai" "pho" 13 3 #t))
;;; (check-expect (rat-sighting (make-Review "r3" "indian" "naan" 2.50 3 #t)) (make-Review "r3" "indian" "naan" 2.50 3 #t))

;;! Problem 3

;; You are in the robot part business, making essential parts for robots.
;; The only parts you make are LIDAR sensors, depth cameras, accelerometers,
;; electric motors, and batteries. For every part, you track the kind of
;; part, the price of the item, and the number of items in stock.

;;! Part A

;; Design data definitions called PartType and Stock to represent
;; a single type of item in stock.

;; A PartType is one of:
;; "LIDAR sensor"
;; "depth camera"
;; "accelerometer"
;; "electric motor"
;; "battery"


;; A Stock is a (make-Stock String Number Number)
;; A part-type is a PartType
;; price is the price of an individual PartType
;; amount is the number of total items in stock

(define-struct Stock (part-type price amount))

;;; (define (Stock-template Stock)
;;;   (...(Stock-price Stock)...(Stock-amount Stock)))

;;! Part B

;; Design a function called discount that takes an Stock and a discount
;; value, and reduces the price by the given value. However, if the price
;; is lower than $10, do not apply the discount. You can assume that the
;; discount is less than the original price.

;; discount: Stock Number
;; Applies a discount to a stock only if the price is greater than or equal to $10
(define (discount stock disc)
  (if (< (Stock-price stock) 10) stock
      (make-Stock (Stock-part-type stock) (-(Stock-price stock)disc) (Stock-amount stock))))



;;; (check-expect (discount (make-Stock "depth camera" 8 6) 9) (make-Stock "depth camera" 8 6))
;;; (check-expect (discount (make-Stock "LIDAR sensor" 111 9) 11) (make-Stock "LIDAR sensor" 100 9))
;;; (check-expect (discount (make-Stock "battery" 30 45) 10) (make-Stock "battery" 20 45) )


;;! Part C

;; Design a function called greater-value? that takes two Stocks and
;; produces #true iff the value (quantity * price) of the first is greater than
;; or equal to the value of the second.
;; Note: To receive full credit, you will need to write a helper function that
;; follows the template.

;; greater-value?: Stock Stock -> Boolean
;; Compares two stocks to see if the value of the first stock is greater
;; than or equal to the value of the second inputted stock 
(define (greater-value? s1 s2)
  (>= (compute-stock-value s1) (compute-stock-value s2))
  )

;;; (check-expect(greater-value? (make-Stock "depth camera" 100 16) (make-Stock "LIDAR sensor" 100 10)) #t)
;;; (check-expect(greater-value? (make-Stock "battery" 4 4) (make-Stock "accelerometer" 4 4)) #t)
;;; (check-expect(greater-value? (make-Stock "electric motor" 9 3) (make-Stock "LIDAR sensor" 10 3)) #f)

;; compute-stock-value: Stock -> Number
;; Computes the total value of a stock by getting the product
;; of the price and the total amount of items in stock
(define (compute-stock-value s) (* (Stock-price s) (Stock-amount s)))


