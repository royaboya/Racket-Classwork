#lang racket

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; Usual Instructions:
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the _design recipe_ for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2023F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get no errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether or Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New Instructions                                                           ;;
;; 1. Many problems have provided signatures and purpose statements that you  ;;
;;    should not modify.                                                      ;;
;; 2. When we write "complete the following function design", you should      ;;
;;    write the function definition and check-expects.                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;! Problem 1

;; Complete the following function design.

;;! average-of-two-lists : [List-of Number] [List-of Number] -> [List-of Number]
;;! Produces a list of numbers where each number is the average of the
;;! corresponding numbers in the two lists.
(define (average-of-two-lists first second)
  (map (lambda (x y) (/ (+ x y) 2)) first second))

(check-expect (average-of-two-lists (list 4 4 4 4) (list 4 4 4 4)) (list 4 4 4 4))
(check-expect (average-of-two-lists (list 2 4 2) (list 4 2 4)) (list 3 3 3))
(check-expect (average-of-two-lists (list 23 17) (list 5 6)) (list 14 11.5))

;;! Problem 2

;; Complete the following function design *without using the builtin function
;; replicate*.

;;! repeat-strings-solo : Nat [List-of String] -> [List-of String]
;;! (repeat-strings-solo n slist) produces a produces a list of strings where
;;! each output string is the corresponding input string repeated n times.
(define (repeat-strings-solo n slist)
  (local [(define (str-repeat str num)
            (cond
              [(> num 1) (string-append str (str-repeat str (- num 1)))]
              [else str]))]
  (map (lambda (str) (str-repeat str n)) slist)))

(check-expect (repeat-strings-solo 1 (list "hello" "there")) (list "hello" "there"))
(check-expect (repeat-strings-solo 2 (list "general" "kenobi" "!")) (list "generalgeneral" "kenobikenobi" "!!"))
(check-expect (repeat-strings-solo 4 (list "Sam" "I am")) (list "SamSamSamSam" "I amI amI amI am"))

;;! Problem 3

;; Complete the following function design *and you may use the builtin
;; replicate.*

;;! repeat-strings : [List-of String] [List-of Nat] -> [List-of String]
;;! (repeat-strings slist nlist) produces a list of strings from slist, where
;;! each is duplicated N times, where N is the corresponding number in
;;! nlist. However:
;;!
;;! 1. If there  are more strings than numbers, assume that the extra strings
;;!    should be repeated twice each.
;;! 2. If there are more numbers than strings, for each extra number N,
;;!    repeat the the string "Extra!" N times.

(define (repeat-strings slist nlist)
  (map (lambda (str num) (replicate num str)) slist nlist))

(check-expect (repeat-strings (list "hello" "there") (list 2 1)) (list "hellohello" "there"))
(check-expect (repeat-strings (list "general" "kenobi" "!") (list 1 2 3)) (list "general" "kenobikenobi" "!!!"))
(check-expect (repeat-strings (list "Sam" "I am") (list 3 3)) (list "SamSamSam" "I amI amI am"))

;;! Problem 4

;; Consider the following data definitions (we have omitted examples and
;; templates).

(define-struct student [name nuid])
;;! A Student is a (make-student String Number)
;;! Interpretation: represents a student

(define-struct grade [nuid course value])
;;! A Grade is a (make-grade Number String Number)
;;! (make-grade nuid course grade) represents the grade that
;;! a student received in a course.

(define-struct student-grades [name grades])
;;! A StudentGrades is a (make-student-grades String [List-of Number]).
;;! (make-student-grades name grades) represents the grades
;;! that a student has received in all courses.

;; Complete the following function design.

;;! students->student-grades: [List-of Student] [List-of Grade] -> [List-of StudentGrades]
;;! Produces a StudentGrade for each student, with the list of grades that
;;! student received. The list produced should have an item for every student in the
;;! input list, even if there are no grades for that student.
(define (students->student-grades stds grds)
  (map (lambda (std)
         (make-student-grades (student-name std) (map grade-value
                                                      (filter (lambda (grd) (= (grade-nuid grd) (student-nuid std)))
                                                              grds))))
       stds))

(check-expect (students->student-grades (list (make-student "jeff" 123123) (make-student "coldclear" 531212))
                                        (list (make-grade 123123 "CS2500" 93) (make-grade 123123 "CS1800" 89) (make-grade 531212 "CS2500" 99)))
              (list (make-student-grades "jeff" (list 93 89)) (make-student-grades "coldclear" (list 99))))
(check-expect (students->student-grades (list (make-student "jeff" 123123) (make-student "coldclear" 531212))
                                        (list (make-grade 123123 "CS2500" 93) (make-grade 123123 "CS1800" 89) (make-grade 203333 "CS2500" 99)))
              (list (make-student-grades "jeff" (list 93 89)) (make-student-grades "coldclear" '())))
(check-expect (students->student-grades (list (make-student "jeff" 123123) (make-student "coldclear" 531212))
                                        '())
              (list (make-student-grades "jeff" '()) (make-student-grades "coldclear" '())))