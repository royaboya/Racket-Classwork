;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;! Lab 8

;;! Part A

;; Design the function pay-rates which, given a list of names as strings
;; and a list of hourly rates as number, produces a list of strings
;; in order which describes how much money each person makes.

;; The list of names will be the same size or longer than the list of
;; rates; people with no corresponding wage are paid $15/hour (Minimum
;; wage in Boston in 2023). Two tests are provided for clarity.

;; pay-rates: [List-of String] [List-of Number] -> [List-of String]
;; Produces a list of descriptions of how much money each person makes.

(check-expect
 (pay-rates
  (list "Archer" "Bella" "Charlie")
  (list 30 20 19))
 (list "Archer is paid $30/hour"
       "Bella is paid $20/hour"
       "Charlie is paid $19/hour"))

(check-expect
 (pay-rates (list "Alex" "Robin") (list 25))
 (list "Alex is paid $25/hour" "Robin is paid $15/hour"))

;; [TODO] Function body
(define (pay-rates names rates)
  (map (lambda (name rate)
         (string-append name " is paid $" (number->string rate) "/hour"))        
       names
       (append rates (make-list (- (length names) (length rates)) 15))))

;;! Part B

;; Design the function alternate which, given two lists
;; produces a list of alternating elements from each list (and if
;; one list runs out of elements you should place all the remaining
;; elements in the other list at the end). Some tests have been
;; supplied for clarity.

;; Restrictions:
;; - Do NOT use ISL list abstractions.

;; alternate: [List-of X] [List-of X] -> [List-of X]
;; Produces a list resulting from alternating between the two
;; supplied lists.

(check-expect
 (alternate '() '())
 '())

(check-expect
 (alternate '() (list 1 "a"))
 (list 1 "a"))

(check-expect
 (alternate (list 1 2 3) '())
 (list 1 2 3))

(check-expect
 (alternate (list 1 2)
            (list "a" "b" "c"))
 (list 1 "a" 2 "b" "c"))

(check-expect
 (alternate (list "a" "b" "c") '())
 (list "a" "b" "c"))

;; [TODO] Function body
(define (alternate l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [(and (cons? l1) (cons? l2)) (cons (first l1) (cons (first l2) (alternate (rest l1) (rest l2))))]))


;;! Part C

;; Design a function that, given two lists of any types,
;; pairs up the corresponding items in order. The first item
;; in each list should be together, then the second items, and
;; so on. These pairs take the form of lists two items long.
;; Any extra elements in either list are ignored.
;; Some tests have been supplied for clarity.

;; pair-lists: [List-of X] [List-of Y] -> [List-of [List-of X/Y]]
;; Pairs up corresponding items in each list in order.

(check-expect (pair-lists (list "A" "B" "C") (list 1 2 3))
              (list (list "A" 1) (list "B" 2) (list "C" 3)))
(check-expect (pair-lists (list "A" "B") (list 1 2 3 4))
              (list (list "A" 1) (list "B" 2)))
(check-expect (pair-lists (list "A" "B" "C" "D" "E") (list 1 2))
              (list (list "A" 1) (list "B" 2)))

;; [TODO] Function body
(define (pair-lists first second)
  (map (lambda (key val)
         (list key val))
       (if (> (length first) (length second))
           (truncate-list first (length second))
           first)
       (if (> (length second) (length first))
           (truncate-list second (length first))
           second)))

(define (truncate-list lst n)
  (cond
    [(or (empty? lst) (zero? n)) '()]
    [else (cons (first lst) (truncate-list (rest lst) (- n 1)))]))
