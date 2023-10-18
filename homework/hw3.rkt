#lang racket

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3_new_copy) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Purpose: Recipe recipe practice, now with unions and self-referential data definitions.

(require 2htdp/image)
(require 2htdp/universe)

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

;; Consider the following structure definitions:
(define-struct blender [brand wattage crushes-ice?])
(define-struct microwave [brand power-level])
(define-struct kettle [brand capacity])
(define-struct toaster [brand slices])

;;! Part A

;; Complete four data designs for each structure called Blender, Microwave,
;; Kettle, and Toaster.


;; A Blender is a (make-blender String Number Boolean)
;; Represents a blender in a kitchen where:
;; - brand is the brand name of the blender
;; - wattage is the amount of wattage of the blender
;; - crushes-ice? is #true if the blender can crush ice

(define BLENDER-ONE (make-blender "Vitamix" 500 #false))
(define BLENDER-TWO (make-blender "Nutribullet" 600 #true))
(define BLENDER-THREE (make-blender "KitchenAid" 675 #true))

(define (blender-template blender)
  (...(blender-brand blender)...(blender-wattage blender)
      ...(blender-crushes-ice? blender)...))

;; A Microwave is a (make-microwave String Number)
;; Represents a microwave in a kitchen where:
;; - brand is the brand name of the brand
;; - power-level is the power level of the microwave

(define MICRO-ONE (make-microwave "Panasonic" 10))
(define MICRO-TWO (make-microwave "Hamilton" 5))
(define MICRO-THREE (make-microwave "Samsung" 4))

(define (microwave-template microwave)
  (...(microwave-brand microwave)...(microwave-power-level microwave)))


;; A Kettle is a (make-kettle String Number)
;; Represents a kettle in a kitchen where:
;; - brand is the brand name of the kettle
;; - capacity is the amount of liquid the kettle can hold

(define KETTLE-ONE (make-kettle "Cello" 100))
(define KETTLE-TWO (make-kettle "Havels" 200))
(define KETTLE-THREE (make-kettle "Butterfly" 250))

(define (kettle-template kettle)
  (... (kettle-brand kettle)...(kettle-capacity kettle)...))

;; A Toaster is a (make-toaster String Number)
;; Represents a toaster in a kitchen where:
;; - brand is the brand name of the toaster
;; - slices is the number of toast slices the toaster can hold

(define TOASTER-ONE (make-toaster "Smeg" 2))
(define TOASTER-TWO (make-toaster "KitchenAid" 3))
(define TOASTER-THREE (make-toaster "Oster" 2))

(define (toaster-template toaster)
  (...(toaster-brand toaster)...(toaster-slices toaster)...))

;;! Part B

;; Complete a data design called Appliance, which can represent any appliance
;; listed above.

;; An Appliance is one of:
;; - (make-blender String Number Boolean)
;; - (make-microwave String Number)
;; - (make-kettle String Number)
;; - (make-toaster String Number)
;; - #false
;; Appliance is #false if there is no appliance currently available
;; Interpretation: Represents an appliance in a kitchen

;; EXAMPLES
(define APPLIANCE-ONE TOASTER-ONE)
(define APPLIANCE-TWO MICRO-ONE)
(define APPLIANCE-THREE #false) 

;;! Part C

;; Complete a data design called Kitchen, which may have 1--3 appliances.
;; (If you have read ahead to lists, do not use lists.)

(define-struct kitchen [first second third])
;; A Kitchen is a (make-kitchen Appliance Appliance Appliance)
;; - first is one of the kitchen's appliances
;; - second is another of the kitchen's appliances
;; - third is the third of the kitchen's appliances
;; Interpretation: A kitchen in a home with 1--3 appliances

(define (kitchen-template kitchen)
  (... (kitchen-first kitchen) ... (kitchen-second kitchen) ... (kitchen-third kitchen) ...))


;; EXAMPLES
(define KITCHEN-ONE (make-kitchen APPLIANCE-ONE APPLIANCE-TWO APPLIANCE-THREE))
(define KITCHEN-TWO (make-kitchen KETTLE-TWO #false #false))
(define KITCHEN-THREE (make-kitchen TOASTER-THREE KETTLE-THREE MICRO-THREE))


;;! Part D

;; Design a function that takes a Kitchen and produces another Kitchen
;; that is identical, except that all microwaves have their power-level
;; incremented by 50.

;; power-up-microwaves: Kitchen -> Kitchen
;; Purpose: Increases the power level of all microwaves in the kitchen by 50
(define (power-up-microwaves Kitchen)
  (make-kitchen (increase-microwave-power (kitchen-first Kitchen))
                (increase-microwave-power (kitchen-second Kitchen))
                (increase-microwave-power (kitchen-third Kitchen))))

(check-expect
 (power-up-microwaves KITCHEN-ONE)
 (make-kitchen TOASTER-ONE (make-microwave "Panasonic" 60) #false))
(check-expect
 (power-up-microwaves KITCHEN-TWO)
 (make-kitchen KETTLE-TWO #false #false))
(check-expect
 (power-up-microwaves KITCHEN-THREE)
 (make-kitchen TOASTER-THREE KETTLE-THREE (make-microwave "Samsung" 54)))


;; increase-microwave-power: Appliance -> Appliance
;; Purpose: If a given appliance is a microwave, its power level is increased by 50. Otherwise, the original appliance is returned
(define (increase-microwave-power app)
  (cond
    [(microwave? app) (make-microwave (microwave-brand app) (+ 50 (microwave-power-level app)))]
    [else app]))

(check-expect (increase-microwave-power BLENDER-TWO) BLENDER-TWO)
(check-expect (increase-microwave-power #false) #false)
(check-expect (increase-microwave-power MICRO-TWO) (make-microwave (microwave-brand MICRO-TWO) (+ (microwave-power-level MICRO-TWO) 50)))

;;! Problem 2

;; You work at a vehicle dealership, and you need to keep track of different
;; types of vehicles: cars, motorcycles, and trucks. For each car, you track
;; its brand, mileage, and number of seats. For each motorcycle, you track its
;; brand, mileage, and engine size. For each truck, you track its brand, mileage
;; and payload capacity.

;;! Part A

;; Complete a data design called Vehicle that can represent any one vehicle.

;; A Type is one of:
;; - "car"
;; - "motorcycle"
;; - "truck"
;; - Represents the type of vehicle

(define CAR "car")
(define MOTORCYCLE "motorcycle")
(define TRUCK "truck")

(define-struct vehicle [type brand mileage specific-capacity])
;; A Vehicle is a (make-vehicle Type String Number Number)
;; - type is the type of the vehicle 
;; - brand is the brand of the vehicle
;; - mileage is the mileage of the vehicle
;; - specific-capacity is the vehicle's specific capacity depending on vehicle type
;; specific-capacity represents the vehicle's number of seats if its type is "car"
;; specific-capacity represents the vehicle's engine size if its type is "motorcycle"
;; specific-capacity represents the vehicle's payload capacity if its type is "truck"

;; EXAMPLES
(define VEHICLE-ONE (make-vehicle CAR "toyota" 130000 4))
(define VEHICLE-TWO (make-vehicle TRUCK "ford" 240000 100))
(define VEHICLE-THREE (make-vehicle MOTORCYCLE "honda" 90000 600))

(define (vehicle-template Vehicle)
  (...(vehicle-type Vehicle)...(vehicle-brand Vehicle)
  ...(vehicle-mileage Vehicle)...(vehicle-specific-capacity Vehicle)))

;;! Part B

;; Design a predicate called `high-mileage?` that determines if a vehicle has
;; is high mileage. Trucks are high-mileage if they have completed more than
;; 250,000 miles, but the others are high-mileage if they have completed more
;; than 100,000 miles.

;; high-mileage?: Vehicle -> Boolean
;; Checks if a given vehicle is a high mileage vehicle or not
(define (high-mileage? Vehicle)
  (cond
    [(and (string=? (vehicle-type Vehicle) TRUCK) (> (vehicle-mileage Vehicle) 250000))#t]
    [(and (not (string=? (vehicle-type Vehicle) TRUCK)) (> (vehicle-mileage Vehicle) 100000)) #t]
    [else #false]))

(check-expect (high-mileage? VEHICLE-ONE) #t)
(check-expect (high-mileage? VEHICLE-TWO) #f)
(check-expect (high-mileage? VEHICLE-THREE) #f)
(check-expect (high-mileage? (make-vehicle TRUCK "honda" 300000 200)) #t)



;;! Part C

;; Design a function with the following signature and purpose statement:

;;! add-miles : Vehicle Number -> Vehicle
;;! Adds the given number of miles to the vehicle's mileage.
(define (add-miles Vehicle miles)
  (make-vehicle (vehicle-type Vehicle) (vehicle-brand Vehicle)
                (+ miles (vehicle-mileage Vehicle)) (vehicle-specific-capacity Vehicle)))

;; not sure if we need to create a separate signature and purpose for part C


;; Test cases
(check-expect (add-miles VEHICLE-ONE 1000) (make-vehicle "car" "toyota" 131000 4))
(check-expect (add-miles VEHICLE-TWO 0) (make-vehicle "truck" "ford" 240000 100))
(check-expect (add-miles VEHICLE-THREE 9000) (make-vehicle "motorcycle" "honda" 99000 600))


;;! Part D

;; Design a function called `describe-vehicle` takes a `Vehicle` and
;; produces one of these strings:
;; - "A car that seats <n> people!"
;; - "A motorcycle with a <n>cc engine!"
;; - "A truck that hauls <n>lbs!"


;; describe-vehicle: Vehicle -> String
;; Purpose Describes the given vehicle and its specific-capacity
(define (describe-vehicle Vehicle)
  (cond
    [(string=? (vehicle-type Vehicle) CAR)
     (string-append "A car that seats " (number->string(vehicle-specific-capacity Vehicle)) " people!")]
    [(string=? (vehicle-type Vehicle) MOTORCYCLE)
     (string-append "A motorcycle with a " (number->string(vehicle-specific-capacity Vehicle)) "cc engine!")]
    [(string=? (vehicle-type Vehicle) TRUCK)
     (string-append "A truck that hauls " (number->string(vehicle-specific-capacity Vehicle)) " lbs!")]))

;; Test cases
(check-expect (describe-vehicle VEHICLE-ONE) "A car that seats 4 people!")
(check-expect (describe-vehicle VEHICLE-TWO) "A truck that hauls 100 lbs!")
(check-expect (describe-vehicle VEHICLE-THREE) "A motorcycle with a 600cc engine!")
              
;;! Problem 3

;; Write a world program that looks and behaves approximately like this:
;;
;; https://pages.github.khoury.northeastern.edu/2500/2023F/starter/hw3_demo.gif
;;
;; The two triangles must be oriented as shown, and they must follow the mouse
;; as shown. Beyond that, feel free to be creative.
;;
;; Your world program should have the following name and signature:

;; target-program : WorldState -> WorldState
;; (define (target-program initial-state)
;;  (big-bang initial-state
;;    ...))

;; (Recall that big-bang produces the final State.)
;;
;; Furthermore:
;; 1. You can define WorldState however you like.
;; 2. When you click Run, the window must *not* appear. i.e., use
;; target-program in Interactions, and not in Definitions.

;; target-program : WorldState -> WorldState
;; Purpose: Creates a world where two triangles on the edge of the screen point towards the user's mouse
(define (target-program posn)
  (big-bang posn
    [to-draw hnd-draw]
    [on-mouse hnd-mouse]))

;; hnd-draw : Posn -> Image
;; Purpose: Draws a scene with triangles pointing to a position
(define (hnd-draw p)
  (place-image
   (rotate 180 (triangle 30 "solid" "red"))
   (posn-x p) 15
   (place-image
    (rotate 270 (triangle 30 "solid" "red"))
    15 (posn-y p)
    (empty-scene 1280 720))))

(check-expect (hnd-draw (make-posn 0 0))
              (place-image
               (rotate 180 (triangle 30 "solid" "red"))
               0 15
               (place-image
                (rotate 270 (triangle 30 "solid" "red"))
                15 0
                (empty-scene 1280 720))))
(check-expect (hnd-draw (make-posn 30 20))
              (place-image
               (rotate 180 (triangle 30 "solid" "red"))
               30 15
               (place-image
                (rotate 270 (triangle 30 "solid" "red"))
                15 20
                (empty-scene 1280 720))))
(check-expect (hnd-draw (make-posn 40 10))
              (place-image
               (rotate 180 (triangle 30 "solid" "red"))
               40 15
               (place-image
                (rotate 270 (triangle 30 "solid" "red"))
                15 10
                (empty-scene 1280 720)))) 

;; hnd-mouse : Posn Number Number MouseEvent
;; Purpose: Returns the position of the mouse
(define (hnd-mouse p x y event)
  (make-posn x y))

(check-expect (hnd-mouse (make-posn 10 10) 11 10 "move") (make-posn 11 10))
(check-expect (hnd-mouse (make-posn 13 10) 22 40 "move") (make-posn 22 40))
(check-expect (hnd-mouse (make-posn 100 200) 200 100 "move") (make-posn 200 100))

