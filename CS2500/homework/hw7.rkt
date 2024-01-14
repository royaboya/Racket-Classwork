#lang racket

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw7final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
;; 1. You must use list abstractions to receive credit. Do not write directly ;;
;;    recursive functions.                                                    ;;
;; 2. You may use Lambda if you wish.                                         ;;
;; 3. Many problems have provided signatures and purpose statements that you  ;;
;;    should not modify.                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This homework refers to the following data designs.

;;! A Category is one of:
;;! - "Personal"
;;! - "Work"
;;! - "Academic"
;;! Interpretation: a category of tasks in a task list.
(define PERSONAL "Personal")
(define WORK "Work")
(define ACADEMIC "Academic")

(define (category-template cat)
  (cond
    [(string=? cat PERSONAL) ...]
    [(string=? cat WORK) ...]
    [(string=? cat ACADEMIC) ...]))

(define-struct task [category description priority])
;; An Task is (make-task Category String Number)
;; Interpretation: A task in a task list, with its category, description, and
;; priority. Lower numbers are more urgent.
(define EX-ASSIGNMENT (make-task ACADEMIC "Finish HW7" 0))
(define EX-LIBRARY (make-task WORK "Finishing shelving books in Snell" 10))
(define EX-PERSONAL (make-task PERSONAL "Do laundry this time" 20))

(define (task-template t)
  (... (task-category t) ... (task-description t) ... (task-priority t) ...))


;; priority-abs: [List-of-Task] (Number Number -> Boolean) Number -> [List-of-Task]
;; List abstraction to change the priorities of a task-list
(define (priority-abs task-list func n)  (filter (lambda (task) (func (task-priority task) n)) task-list) )

;; EXAMPLE LISTS FOR TESTING
(define TASK-LIST-1 (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))
(define TASK-LIST-2 (list EX-ASSIGNMENT EX-LIBRARY))
(define TASK-LIST-3 (list EX-ASSIGNMENT))
(define TASK-LIST-4 (list EX-ASSIGNMENT EX-ASSIGNMENT EX-ASSIGNMENT EX-LIBRARY))
(define TASK-LIST-EMPTY '())


;;! Problem 1

;; Design a function called priority-zero that consumes a list of tasks and
;; only produces those with priority 0.

;;! priority-zero : [List-of Task] -> [List-of Task]
;;! Produces a list of tasks with priority 0.
(define (priority-zero task-list) (priority-abs task-list = 0))

(check-expect (priority-zero TASK-LIST-4) (list EX-ASSIGNMENT EX-ASSIGNMENT EX-ASSIGNMENT))
(check-expect (priority-zero TASK-LIST-EMPTY) '())
(check-expect (priority-zero (list EX-LIBRARY EX-PERSONAL)) '())

;;! Problem 2

;; Design a function called priority<= that consumes a priority number and
;; a list of tasks and produces only those with priority less than or equal
;; to the given number.

;;! priority<= : Number [List-of Task] -> [List-of Task]
;;! Produces a list of tasks with priority less than or equal to the given
;;! number.
(define (priority<= n task-list) (priority-abs task-list <= n))

(check-expect (priority<= 10 TASK-LIST-2) (list EX-ASSIGNMENT EX-LIBRARY))
(check-expect (priority<= 15 TASK-LIST-1) (list EX-ASSIGNMENT EX-LIBRARY))
(check-expect (priority<= 10 TASK-LIST-EMPTY) '())


;; prioritize-abs: [List-of-Task] Number -> [List-of-Task]
;; sets all tasks of a task list to priority n
(define (prioritize-abs task-list n)
  (map (lambda (task)
         (make-task (task-category task) (task-description task) n))
       task-list))

(check-expect (prioritize-abs TASK-LIST-1 0) (list EX-ASSIGNMENT (make-task WORK "Finishing shelving books in Snell" 0) (make-task PERSONAL "Do laundry this time" 0)))
(check-expect (prioritize-abs TASK-LIST-3 10) (list (make-task ACADEMIC "Finish HW7" 10)))
(check-expect (prioritize-abs TASK-LIST-EMPTY 10) '())

;;! Problem 3

;; Design a function called prioritize that consumes a category and a list of
;; tasks, and sets the priority of all tasks in the given category to 0. The
;; produced list should contain all tasks in the original list.

;;! prioritize : Category [List-of Task] -> [List-of Task]
;;! Produces every task in the given list of tasks. But, sets the priority of
;;! tasks in the given category to zero.
(define (prioritize cat task-list)
  (map (lambda (task)
         (if (string=? (task-category task) cat)
             (make-task cat (task-description task) 0)
             task))
       task-list))

(check-expect (prioritize PERSONAL TASK-LIST-1) (list EX-ASSIGNMENT EX-LIBRARY (make-task PERSONAL "Do laundry this time" 0)))
(check-expect (prioritize WORK TASK-LIST-2) (list EX-ASSIGNMENT (make-task WORK "Finishing shelving books in Snell" 0)))
(check-expect (prioritize ACADEMIC TASK-LIST-EMPTY) '())

;;! Problem 4

;; Design a predicate called any-work? that determines if any task in a list
;; is a work task.

;;! any-work? : [List-of Task] -> Boolean
;;! Determines if any task in the given list is a work task.
(define (any-work? task-list)
  (ormap
   (lambda (task) (string=? WORK (task-category task)))
   task-list))

(check-expect (any-work? TASK-LIST-1) #t)
(check-expect (any-work? TASK-LIST-3) #f)
(check-expect (any-work? TASK-LIST-EMPTY) #f)

;;! Problem 5

;; Design a function called count-academic that consumes a list of tasks and
;; produces the number of tasks in the list that are academic tasks.

;;! count-academic : [List-of Task] -> Number
;;! Produces the number of tasks in the given list that are academic tasks.
(define (count-academic task-list)
  (length (filter
           (lambda (task) (string=? ACADEMIC (task-category task)))
           task-list)))

(check-expect (count-academic TASK-LIST-1) 1)
(check-expect (count-academic TASK-LIST-4) 3)
(check-expect (count-academic TASK-LIST-EMPTY) 0)

;;! Problem 6

;; Design a function called search that consumes a list of tasks and a string
;; and produces a list of tasks whose description contains the given string.

;;! search : String [List-of Task] -> [List-of Task]
;;! Produces a list of tasks whose description contains the given string.
(define (search target task-list)
  (filter
   (lambda (task) (string-contains? target (task-description task)))
   task-list))

(check-expect (search "Snell" TASK-LIST-1) (list EX-LIBRARY))
(check-expect (search "Finnish" TASK-LIST-1) '())
(check-expect (search " " TASK-LIST-4) TASK-LIST-4)


;;! Problem 7

;; Design a function called search-work that consumes a string and produces
;; the descriptions of the work tasks that contain the given string.

;;! search-work : String [List-of Task] -> [List-of String]
;;! Produces a list of descriptions of work tasks that contain the given string.
(define (search-work target task-list)
  (filter
   (lambda (task) (string=? (task-category task) WORK))
   (search target task-list)))

(check-expect (search-work "Snell" TASK-LIST-1) (list EX-LIBRARY))
(check-expect (search-work "Finnish" TASK-LIST-1) '())
(check-expect (search-work " " TASK-LIST-4) (list EX-LIBRARY))