#lang racket


(define-struct Book [title author borrow-count borrower due stacks?])
;; A Book is a (make-Book String String Number String)
;; title is the title of the book
;; author is the author of the book
;; borrow-count is the number of times the book was borrowed (>= 0)
;; borrower is the person who currently has it checked out
;; due is true or false depending on if the book is past due
;; stacks is if the book is on the stacks
;; Interpretation: A library book

(define BOOK1 (make-Book "The Giving Tree" "Shel Silverstein" 10 "Brian" #true))
(define BOOK2 (make-Book "Title two" "Unknown" 5 "Ryan" #false))
(define BOOK3 (make-Book "Title three" "Unknown" 0 "Campbell" #true))
(define BOOK4 (make-Book "Title three" "Unknown" 0 "" #false))

;; commenting out because vscode does not like ellipses
#|
(define (Book-template book)
    (...(Book-Title book)...(Book-author book)
     ...(Book-borrow-count book)...))
|#

;; Design a function to determine if a book is popular (borrowed 10+ times)

;; popular: Book -> Boolean
;; Returns whether it is true or false that a book is popular
(define (popular? book) (>= (Book-borrow-count book) 10))

;; Commented out check expects because vscode bad
#|
(check-expect(popular? BOOK1) #t)
(check-expect(popular? BOOK2) #f)
(check-expect(popular? BOOK3) #f)
|#


;; Design a function that consumes a book and produces the name
;; of the person who has borrowed it. If it is not checked out, it
;; should produce #false.


;; check-borrow-status: Book -> String U Boolean 
;; Checks whether or not a book is due and returns the name of the 
;; borrower if the book is late
(define (check-borrow-status book)
  (if (not (string=? (Book-borrower book) "")) (Book-borrower book) #false))

;; Design a predicate that determines if a book is checked out and late

;; is-late: Book -> Boolean
;; Checks whether or not a book is or is not late
(define (is-late? book)
  (and (not(string=? (Book-borrower book) "")) (Book-due book)))

;; Design a function that describes a book. It should produce a string 
;; with its title and whether it is checked out, on the stacks, or 
;; returned (and waiting to be put back on the stacks)

;; too lazy atm to add stacks property 
(define (describe-book book) 

("no clue yet")


)

;; TAKEAWAY: TO SIMPLIFY AND REDUCE ARGUMENTS
;; YOU CAN DEFINE A NEW DATA DEFINITION THAT COVERS
;; THE STATES OF THE BOOK