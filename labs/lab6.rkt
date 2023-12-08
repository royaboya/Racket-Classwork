;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;! Problem 1

;; Within this lab you will be asked to design several functions that employ the following
;; data designs. The functions you create MUST use the list abstractions when appropriate.
;; Try your best to use the most effective and efficient list abstraction for each part.
;; You may need helper functions for some of the parts. All of the given tests should pass.
;; Data Designs (do not modify these)

;; A CallType is one of:
;; - "zoom"
;; - "teams"
;; - "phone"
;; Interpretation: a type of call

(define CT-ZOOM "zoom")
(define CT-TEAMS "teams")
(define CT-PHONE "phone")

#;(define (calltype-temp ct)
  (cond
    [(string=? ct CT-ZOOM) ...]
    [(string=? ct CT-TEAMS) ...]
    [(string=? ct CT-PHONE) ...]))

(define-struct call [type duration attendees description])
(define-struct mtg [duration attendees description])
(define-struct alone [duration description])

;; An Event is one of:
;; - (make-call CallType PosInt [NEList-of String] String)
;; - (make-mtg PosInt [NEList-of String] String)
;; - (make-alone PosInt String)
;; Interpretation: an event in some period of time, which is either:
;; - A call using some technology, lasting some number of minutes with attendees
;;  (by name), and a description;
;; - An in-person meeting lasting some number of minutes
;;   with attendees (by name) and a description; or
;; - Time spent alone for some number of minutes with a description.

(define E-ZOOM-DOC
  (make-call CT-ZOOM 22 (list "Dr. Zoidberg")
             "Doctor appointment about a stomach ache after some bad seafood :("))
(define E-TEAMS-OH
  (make-call CT-TEAMS 7 (list "Mike" "Tajel")
             "Office hours with my partner to ask clarifying questions about the Design Recipe!"))
(define E-PHONE-SPAM
  (make-call CT-PHONE 1 (list "Unknown")
             "Who calls!? I think it was a scam..."))
;; These are characters from a TV show called "Friends", which was popular in
;; the 90s, which is when many of your instructors grew up.
(define E-MTG-STUDY
  (make-mtg 62 (list "Rachel" "Ross" "Joey" "Phoebe" "Chandler" "Monica")
            "Getting ahead on studying for Exam 2!"))
(define E-MTG-ADVISOR
  (make-mtg 28 (list "Ali")
            "Meeting with advisor to talk about a combined major"))
(define E-ALONE-LUNCH
  (make-alone 34 "Lunch"))
(define E-ALONE-READING
  (make-alone 25 "The Three-Body Problem isn't going to read itself!"))
(define LOE-1
  (list E-ZOOM-DOC E-ALONE-READING E-PHONE-SPAM
        E-ALONE-LUNCH E-TEAMS-OH E-MTG-ADVISOR E-MTG-STUDY))

#;(define (e-temp e)
  (cond
    [(call? e)
     (... (calltype-temp (call-type e)) ...
          (call-duration e) ...
          (los-temp (call-attendees e)) ...
          (call-description e) ...)]
    [(mtg? e)
     (... (mtg-duration e) ...
          (los-temp (mtg-attendees e)) ...
          (mtg-description e) ...)]
    [(alone? e)
     (... (alone-duration e) ...
          (alone-description e) ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! Part A

;; Design a function called weekend that reminds you to rest. The function
;; consumes an argument -- a positive integer -- and produces a list of that
;; many alone events. Each of these is 30mins each, with the description "rest".

;; [TODO] Function design. Use list abstractions when appropriate.

;; weekend: PosInt -> [List-of Event]
;; produces a list of alone events for the weekend

(define (weekend rest-periods)
  (make-list rest-periods (make-alone 30 "rest")))


(check-expect (weekend 2)
              (list (make-alone 30 "rest") (make-alone 30 "rest")))
(check-expect (weekend 3)
              (list (make-alone 30 "rest") (make-alone 30 "rest") (make-alone 30 "rest")))
(check-expect (weekend 4)
              (list (make-alone 30 "rest") (make-alone 30 "rest")
                    (make-alone 30 "rest") (make-alone 30 "rest")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! Part B

;; Design the function small-group that consumes a list of events and only
;; produces those that have fewer than three participants. In all cases,
;; there is an implied attendee (you!) that counts as one person. So,
;; calls and meetings with two or more attendees are *not* small groups.

;; [TODO] Function design. Use list abstractions when appropriate.

;; small-group: [List-of Event] -> [List-of Event]
;; produces a list of events that have less than 3 participants (2 or more attendees)
(define (small-group events)
  (cond
    [(empty? events) empty]
    [(alone? (first events)) (cons (first events) (small-group (rest events)))]
    [(call? (first events))
     (if (< (length (call-attendees (first events))) 2)
      (cons (first events) (small-group (rest events)))
      (small-group (rest events)))]
    [(mtg? (first events))
     (if (< (length (mtg-attendees (first events))) 2)
      (cons (first events) (small-group (rest events)))
      (small-group (rest events)))]))

(check-expect (small-group (list (make-alone 30 "rest")
                                 (make-mtg 30 (list "Dr. Zoidberg") "office hours")))
              (list (make-alone 30 "rest")
                    (make-mtg 30 (list "Dr. Zoidberg") "office hours")))
(check-expect (small-group (list (make-call "zoom" 30 (list "Dr. Zoidberg" "Arjun Guha")
                                            "co-op interview")))
              '())
(check-expect (small-group (list (make-alone 30 "rest")
                                 (make-mtg 30 (list "Dr. Zoidberg" "Arjun Guha") "office hours")
                                 (make-call "zoom" 30 (list "Dr. Zoidberg") "co-op interview")))
              (list (make-alone 30 "rest")
                    (make-call "zoom" 30 (list "Dr. Zoidberg") "co-op interview")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! Part C

;; Design the function had-lunch? that accepts a list of events and determines
;; if it contains an event whose description contains the word "lunch"
;; (without regard for upper/lower-case).

;; [TODO] Function design. Use list abstractions when appropriate.

;; had-lunch?: [List-of Event] -> Boolean
;; determines if any of the given events have a lunch
(define (had-lunch? events)
  (cond
    [(empty? events) #f]
    [(alone? (first events))
     (if (string=? (alone-description (first events)) "lunch")
     #t
     (had-lunch? (rest events)))]
    [(call? (first events))
     (if (string=? (call-description (first events)) "lunch")
     #t
     (had-lunch? (rest events)))]
    [(mtg? (first events))
     (if (string=? (mtg-description (first events)) "lunch")
     #t
     (had-lunch? (rest events)))])
  )

(check-expect (had-lunch? (list (make-alone 30 "lunch")))
              #t)
(check-expect (had-lunch? (list (make-mtg 30 (list "Dr. Zoidberg") "office hours")))
              #f)
(check-expect (had-lunch? (list (make-mtg 30 (list "Dr. Zoidberg") "office hours")
                                (make-alone 30 "lunch")))
              #t)
(check-expect (had-lunch? (list (make-mtg 30 (list "Dr. Zoidberg") "office hours")
                                (make-call "zoom" 30 (list "Dr. Zoidberg") "co-op interview")))
              #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! Part D

;; Design the function social-time that accepts a list of events and produces
;; the total minutes spent on calls or meetings.

;; [TODO] Function design. Use list abstractions when appropriate.

;; social-time: [List-of Event] -> Number
;; calculates the total time in minutes spent in calls or meetings
(define (social-time events)
  (cond
    [(empty? events) 0]
    [(alone? (first events)) (+ 0 (social-time (rest events)))]
    [(call? (first events))
     (+ (call-duration (first events)) (social-time (rest events)))]
    [(mtg? (first events))
     (+ (mtg-duration (first events)) (social-time (rest events)))]))

(check-expect (social-time (list (make-alone 30 "lunch")))
              0)
(check-expect (social-time (list (make-alone 30 "lunch")
                                 (make-mtg 30 (list "Dr. Zoidberg") "office hours")))
              30)
(check-expect (social-time (list (make-alone 30 "lunch")
                                 (make-mtg 30 (list "Dr. Zoidberg") "office hours")
                                 (make-call "zoom" 30 (list "Dr. Zoidberg") "co-op interview")))
              60)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! Part E

;; Design the function anything-but-phone? that accepts a list of events and
;; and produces #true if none of the calls were phone calls. (Zoom and Teams
;; calls are not phone calls.)

;; [TODO] Function design. Use list abstractions when appropriate.

;; anything-but-phone?: [List-of Event] -> Boolean
;; determines if all of the events are not phone calls
(define (anything-but-phone? events)
  (cond
    [(empty? events) #t]
    [(call? (first events)) #f]
    [else (anything-but-phone? (rest events))]))

(check-expect (anything-but-phone? (list (make-alone 30 "lunch")))
              #t)
(check-expect (anything-but-phone? (list (make-alone 30 "lunch")
                                         (make-mtg 30 (list "Dr. Zoidberg") "office hours")))
              #t)
(check-expect (anything-but-phone? (list (make-alone 30 "lunch")
                                         (make-mtg 30 (list "Dr. Zoidberg") "office hours")
                                         (make-call "phone" 30 (list "Dr. Zoidberg")
                                                    "co-op interview")))
              #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! Part F

;; Design the function peeps that accepts a list of
;; events and produces an alphabetically sorted list of attendees
;; at all the calls/meetings (including any duplicates).

;; [TODO] Function design. Use list abstractions when appropriate.

;; peeps: [List-of Event] -> [NEList-of String]
;; creates an alphabetically sorted list of attendees from all given events
(define (peeps events) 
  (cond
    [(empty? events) empty]
    [(alone? (first events)) (peeps (rest events))]
    [(call? (first events)) (sort (append (call-attendees (first events)) (peeps (rest events))) string<?)]
    [(mtg? (first events)) (sort (append (mtg-attendees (first events)) (peeps (rest events))) string<?)]))

(check-expect (peeps (list (make-alone 30 "lunch")))
              '())
(check-expect (peeps (list (make-alone 30 "lunch")
                           (make-mtg 30 (list "Dr. Zoidberg") "office hours")
                           (make-call "zoom" 30 (list "Arjun Guha") "fundies meeting")))
              (list "Arjun Guha" "Dr. Zoidberg"))
(check-expect (peeps (list (make-alone 30 "lunch")
                           (make-mtg 30 (list "Dr. Zoidberg") "office hours")))
              (list "Dr. Zoidberg"))

