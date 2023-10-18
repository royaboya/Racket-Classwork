;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |hw4 (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;! Problem 1

(define-struct person [name next])
;;! A Line is one of:
;;! - "end of line"
;;! - (make-person String Line)
;;! Interpretation: A line of poeple.

(define LINE-EX-1 "end of line")
(define LINE-EX-2 (make-person "Alice" "end of line"))
(define LINE-EX-3 (make-person "Alice" (make-person "Bob" "end of line")))

;;! Part A

;; Write the template for Line.

(define (line-template l)
  (cond
    [(person? l) (... (person-name l) ...
                 (line-template (person-next l)) ...)]
    [(string=? l "end of line") ...]))

;;! Part B

;; Design a function called count-people that counts the number of people in
;; a line.

;; count-people: Line -> Number
;; Counts the number of people in line
(define (count-people l)
  (cond
    [(person? l) (+ 1 (count-people (person-next l)))]
    [else 0]))

(check-expect (count-people LINE-EX-1) 0)
(check-expect (count-people LINE-EX-2) 1)
(check-expect (count-people LINE-EX-3) 2)

;;! Part C

;; Design a predicate called waldo-in-line? that determines if a person named
;; "Waldo" is in the line.

;; test with this (make-person "LOL" (make-person "LOL" (make-person "LOL" (make-person "LOL" (make-person "Waldo")))))
;; test with empty case/single case, etc

;; waldo-in-line: Line -> Boolean
;; Checks if there is an instance of "Waldo" in the Line
(define (waldo-in-line? l)
  (cond
    [(not (person? l)) #f]
    [(string=? "Waldo" (person-name l)) #true]
    [else (waldo-in-line? (person-next l))]))

(check-expect (waldo-in-line? (make-person "Alice" (make-person "Bob" (make-person "Waldo" "end of line")))) #t)
(check-expect (waldo-in-line? (make-person "Waldo" (make-person "Bob" (make-person "Alice" "end of line")))) #t)
(check-expect (waldo-in-line? (make-person "Alice" (make-person "Bob" "end of line"))) #f)
(check-expect (waldo-in-line? LINE-EX-1) #f)

;;! Part D

;; Design a function that removes the first "Waldo" in the line. It should have the
;; signature remove-waldo : Line -> Line.

(define TEST (make-person "LOL" (make-person "Waldo" (make-person "LOL" (make-person "LOL" (make-person "lol" "end of line"))))))
(define TEST-2 (make-person "LOL" (make-person "Waldo" (make-person "Waldo" (make-person "LOL" (make-person "lol" "end of line"))))))
(define TEST-3 (make-person "LOL" (make-person "Waldo" (make-person "LOL" (make-person "LOL" (make-person "Waldo" "end of line"))))))
(define TEST-4 (make-person "Waldo" "end of line"))

;; remove-waldo: Line -> Line
;; Removes the first Waldo from the Line
(define (remove-waldo l)
  (cond
    [(not (person? (person-next l))) "end of line"]
    [(string=? "Waldo" (person-name l))
     (make-person (person-name (person-next l)) (person-next (person-next l)))]
    [else (make-person (person-name l) (remove-waldo (person-next l)))]))

;; CHECK EXPECTS
(check-expect (remove-waldo TEST) (make-person "LOL" (make-person "LOL" (make-person "LOL" (make-person "lol" "end of line")))))
(check-expect (remove-waldo TEST-2) (make-person "LOL" (make-person "Waldo" (make-person "LOL" (make-person "lol" "end of line")))))
(check-expect (remove-waldo TEST-3) (make-person "LOL" (make-person "LOL" (make-person "LOL" (make-person "Waldo" "end of line")))))
(check-expect (remove-waldo TEST-4) "end of line")

;;! Problem 2


(define-struct quest-entry [name completed next])
;;! A QuestLog is one of:
;;! - "empty"
;;! - (make-quest-entry String Boolean QuestLog)
;;! Interpretation: A quest log where each entry contains a quest name and a
;;! boolean that indicates if that quest is completed.

;;! Part A

;; Complete the data design for QuestLog (examples and template)

(define (quest-entry-template q)
  (cond
    [(quest-entry? q) (... (quest-entry-name q) ... (quest-entry-completed q) ...
                (quest-entry-template (quest-entry-next q)) ...)]
    [(string=? q)...]))

(define QUEST-LOG-1 (make-quest-entry "Groceries" #true (make-quest-entry "Hungry Hippo" #false (make-quest-entry "Kill Dragon" #true "empty"))))
(define QUEST-LOG-2 (make-quest-entry "Dead Man" #false (make-quest-entry "Rolfenne's sword" #false "empty")))
(define QUEST-LOG-3 "empty")

;;! Part B

;; Design a function called count-completed-quests that counts the number of
;; completed quests in a quest log.

;; count-completed-quests: QuestLog -> Number
;; Counts the total number of completed quest logs in a QuestLog
(define (count-completed-quests q)
  (cond
    [(and (quest-entry? q) (quest-entry-completed q)) (+ 1 (count-completed-quests (quest-entry-next q)))]
    [(quest-entry? q) (count-completed-quests (quest-entry-next q))]
    [(not (quest-entry? q)) 0]))

(check-expect (count-completed-quests QUEST-LOG-1) 2)
(check-expect (count-completed-quests QUEST-LOG-2) 0)
(check-expect (count-completed-quests QUEST-LOG-3) 0)

;;! Part C

;; Design a function that consumes a QuestLog and only produces the incomplete
;; quests. It should have the signature incomplete-quests : QuestLog -> QuestLog.

;; incomplete-quests: QuestLog -> QuestLog
;; Takes a QuestLog and returns a new QuestLog with only the incomplete quests
(define (incomplete-quests q)
  (cond
    [(and (quest-entry? q) (not (quest-entry-completed q)))
     (make-quest-entry (quest-entry-name q) #false (incomplete-quests (quest-entry-next q)))]
    [(quest-entry? q) (incomplete-quests (quest-entry-next q))]
    [else "empty"]))


(check-expect (incomplete-quests QUEST-LOG-1) (make-quest-entry "Hungry Hippo" #false "empty"))
(check-expect (incomplete-quests QUEST-LOG-2) (make-quest-entry "Dead Man" #false (make-quest-entry "Rolfenne's sword" #false "empty")))
(check-expect (incomplete-quests QUEST-LOG-3) "empty")
(check-expect (incomplete-quests (make-quest-entry "Dead Man" #true (make-quest-entry "Rolfenne's sword" #true "empty"))) "empty")

;;! Part D

;; Design a function that consumes a QuestLog and produces a new QuestLog with
;; the same quests, but all marked completed.


;; mark-all-completed: QuestLog -> QuestLog
;; Returns a QuestLog with all quests from the original QuestLog marked complete
(define (mark-all-completed q)
  (cond
    [(quest-entry? q) (make-quest-entry (quest-entry-name q) #true (mark-all-completed (quest-entry-next q)))]
    [(not (quest-entry? q)) "empty"]))

(check-expect (mark-all-completed QUEST-LOG-1) (make-quest-entry "Groceries" #true (make-quest-entry "Hungry Hippo" #true (make-quest-entry "Kill Dragon" #true "empty"))))
(check-expect (mark-all-completed QUEST-LOG-2) (make-quest-entry "Dead Man" #true (make-quest-entry "Rolfenne's sword" #true "empty")))
(check-expect (mark-all-completed QUEST-LOG-3) "empty")

;;! Problem 3

;; This problem has a partially-completed data definition that represents a
;; workout sequence.

(define-struct cardio [rest])
(define-struct strength [rest])
(define-struct flexibility [rest])
;;! A Workout is one of:
;;! - (make-cardio Workout)
;;! - (make-strength Workout)
;;! - (make-flexibility Workout)
;;! - "done"
;;! Interpretation: A list of exercises in a long workout.

;;! Part A

;; Give three examples of Workouts.
(define WORKOUT-1 (make-cardio (make-strength "done")))
(define WORKOUT-2 (make-flexibility (make-strength "done")))
(define WORKOUT-3 (make-strength (make-flexibility (make-cardio "done"))))

;;! Part B

;; Write the template for Workouts.

(define (workout-template w)
  (cond
    [(cardio? w) (... (workout-template (cardio-next w) ...))]
    [(strength? w) (... (workout-template (strength-next w) ...))]
    [(flexibility? w) (... (workout-template (flexibility-next w) ...))]
    [(string=? w "done") ...]))


;;! Part C

;; Design a function called recovery-sequence to generate the "recovery" sequence for a given
;; Workout. In the recovery sequence, cardio exercises become flexibility
;; exercises, strength exercises become cardio exercises, and flexibility
;; exercises become strength exercises.


;; recovery-sequence: Workout -> Workout
;; Produces a new Workout that is the recovery sequence of the original Workout
;; In the recovery sequence:
;; Cardio becomes flexibility
;; Strength becomes cardio
;; Flexibility becomes strength
(define (recovery-sequence w)
  (cond
    [(cardio? w) (make-flexibility (recovery-sequence (cardio-rest w)))]
    [(strength? w) (make-cardio (recovery-sequence (strength-rest w)))]
    [(flexibility? w) (make-strength (recovery-sequence (flexibility-rest w)))]
    [(string? w) "done"]))

(check-expect (recovery-sequence WORKOUT-1) (make-flexibility (make-cardio "done")))
(check-expect (recovery-sequence WORKOUT-2) (make-strength (make-cardio "done")))
(check-expect (recovery-sequence WORKOUT-3) (make-cardio (make-strength (make-flexibility "done"))))

