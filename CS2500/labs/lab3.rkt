#lang racket

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;;! Problem 1

;;! Part A

;; TODO: design the data necessary to represent a book, which can
;; either be physical or electronic. All books have a title and author.
;; Physical books are either paperback or harcover, and have some number of
;; pages. Electronic (e-books) have a format (pdf, epub, txt) and a source URL.

(define-struct book [type title author pages url])
;; - type can be (paperback, hardcover, pdf, epub, txt)
;; - title is title of book
;; - author is author of book
;; - pages is # of pages in book
;; - url is a the link to the ebook. "" if not an Ebook

(define EX-1 (make-book "paperback" "charlottes web" "i forgot" 250 ""))
(define EX-2 (make-book "pdf" "title 2" "i forgot" 250 ""))
(define EX-3 (make-book "paperback" "charlottes web" "i forgot" 250 ""))

(define (book-template b)
  (...(book-type b)...(book-title b)...(book-author b)...(book-pages b)...(book-url b)...))


;;! Part B

; TODO: now design the function where-to-find that
; accepts a book and returns where you can find it:
; physical books are either in the "hardcover section"
; or "paperback section", whereas electronic books are
; found at their URL.


;; where-to-find: Book-> String
;; Returns the location of a book
(define (where-to-find b)
  (cond
    [(e-helper b) (book-url b)]
    [else (string-append (book-type b) " section")]))

;; e-helper: Book -> Boolean
;; Checks if a book is electronic
(define (e-helper b)
  (or (string=? (book-type b) "pdf" ) (string=? (book-type b) "epub" ) (string=? (book-type b) "txt" )))


;;! Problem 2

;; Consider the follow structure definitions:
(define-struct meme [name dank? upvotes])
(define-struct animal [name generative-ai? upvotes])
(define-struct automobile [name upvotes])

;;! Part A

;; Complete the data designs for each structure: Meme, Animal, and Automobile

;; A Meme is a (make-meme  String Boolean Number)
;; Represents an internet meme where
;; - name is the name of the meme
;; - dank is whether or not the meme is dank
;; - upvotes is the number of upvotes the meme has

;; An Animal is a (make-animal Name Boolean Number)
;; Represents an animal on the internet where
;; - name is the name of the animal
;; - generative-ai? is whether or not it was created by an ai
;; - upvotes is the number of upvotes the animal has

;; An Automobile is a (make-automobile String Number)
;; Represents an automobile where
;; - name is the name of the automobile
;; - upvotes is the number of upvotes it has

(define MEME (make-meme "TEST" #false 123))
(define ANIMAL (make-animal "TEST" #true 1000))
(define AUTO (make-automobile "TEST" 13))
(define MEME2 (make-meme "TEST" #false 100))


;;! Part B

;; Complete a data design called Post, which can represent any of the
;; posts listed above.

;; A post is one of:
;; Represents an internet post where
;; Animal
;; Meme
;; Automobile

;;! Part C

;; Consider this definition:

(define-struct subreddit [first second third fourth])
;; A Subreddit is a (make-subreddit Post Post Post Post)
;; which represents a Subreddit that has 4 posts uploaded.

(define (subreddit-template subred)
  (...
   (subreddit-first subred) ...
   (subreddit-second subred) ...
   (subreddit-third subred) ...
   (subreddit-fourth subred) ... ))

(define S(make-subreddit MEME MEME2 AUTO ANIMAL))

(define (post-copy p)
  (cond
    [(automobile? p) (make-automobile (automobile-name p) (automobile-upvotes p))]
    [(meme? p) (make-meme (meme-name p) (meme-dank? p) (meme-upvotes p))]
    ))


(define (handle-post p)
  (if (and (animal? p) (animal-generative-ai? p))
      (make-animal (animal-name p) (animal-generative-ai? p) (- (animal-upvotes p) 100))
      (post-copy p)))


;; Design a function adjust-upvotes that takes in a Subreddit
;; and produces a Subreddit with all the upvotes of
;; the posts have increased by 150.
;; However, if the post is an animal post and the picture was generated
;; by AI, then the upvotes should decrease by 100.


;; adjust-upvotes: Subreddit -> Subreddit
;; Increases votes of the posts of a subreddit by 150. If it is an animal post and
;; was made by generative ai, then the number of upvotes is decreased by 100
(define (adjust-upvotes subred)
  (make-subreddit
  (handle-post (subreddit-first subred))
  (handle-post (subreddit-second subred))
  (handle-post (subreddit-third subred))
  (handle-post (subreddit-fourth subred))))

