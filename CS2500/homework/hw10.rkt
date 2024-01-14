;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct manager [name team])
(define-struct ic [name])
(define-struct team [name members])

;;! A Manager is a (make-manager String Team)
;;! Interpretation: a manager at a company who directly manages the team.

;;! A Person is one of:
;;! - (make-manager String Team)
;;! - (make-ic String)
;;! Interpretation: A person at a company who is either a manager or an an
;;! individual contributor ("IC").

;;! A Team is a (make-team String [List-of Person])
;;! Interpretation: A team at a company, with a name and a list of members.

;;! Problem 1

;; Define three examples of Manager. One of them should have someone with exactly
;; the same name at two levels of the hierarchy, because that is a common source
;; of confusion at large companies.

(define MNGR-EX1 (make-manager "Edelgard" (make-team "Zendesk Agents" (list (make-ic "Samson") (make-ic "Serverus")))))
(define MNGR-EX2 (make-manager "Edelgard" (make-team "Customer Service" (list (make-ic "Lucia") (make-ic "Gordon") MNGR-EX1))))
(define MNGR-EX3 (make-manager "Astra" (make-team "HR" '())))
(define MNGR-EX4 (make-manager "Edelgard" (make-team "Customer Service" (list (make-manager "Edelgard" (make-team "HR" (list (make-ic "Jesse") MNGR-EX2)))
                                                                              (make-ic "Lucia")
                                                                              (make-ic "Gordon")
                                                                              (make-manager "Sean" (make-team "QA" (list (make-ic "James") MNGR-EX2)))
                                                                              (make-manager "Astra" (make-team "Writers" (list (make-ic "James"))))))))
(define MNGR-EX5 (make-manager "Edelgard" (make-team "Customer Service" (list (make-manager "Edelgard" (make-team "HR" (list (make-ic "Jesse") MNGR-EX2)))
                                                                              (make-ic "Lucia")
                                                                              (make-ic "Gordon")
                                                                              (make-manager "Sean" (make-team "QA" (list (make-ic "James") (make-manager "Astra" (make-team "Dum" (list (make-ic "Serverus")))))))
                                                                              (make-manager "Astra" (make-team "Writers" (list (make-ic "James"))))))))

;;! Problem 2

;; Complete the following function design.

;;! list-direct-reports : String Manager -> [List-of String]
;;! Produces a list of all the direct reports of the manager with the given
;;! name. When several managers have the same name, all of them are included.
(define (list-direct-reports name mngr)
  (local
    [(define (get-ic-reports mngr)
       (filter ic? (team-members (manager-team mngr))))
     (define (get-manager-reports mngr)
       (filter manager? (team-members (manager-team mngr))))
     (define (member->name member)
       (cond
         [(ic? member) (ic-name member)]
         [(manager? member) (manager-name member)]
         [else member]))
     (define (list-members name mngr)
       (cond
         [(empty? (get-manager-reports mngr)) (if (string=? (manager-name mngr) name) (get-ic-reports mngr) '())]
         [(string=? (manager-name mngr) name)
          (append (get-ic-reports mngr)
                  (get-manager-reports mngr)
                  (list-direct-reports name (first (get-manager-reports mngr)))
                  (list-direct-reports name (make-manager (manager-name mngr)
                                                          (make-team (team-name (manager-team mngr))
                                                                     (filter (lambda (manager) (string=? (manager-name manager) name)) (rest (get-manager-reports mngr)))))))]
         [(not (string=? (manager-name mngr) name))
          (append (list-direct-reports name (first (get-manager-reports mngr)))
                  (list-direct-reports name (make-manager (manager-name mngr)
                                                          (make-team (team-name (manager-team mngr))
                                                                     (filter (lambda (manager) (string=? (manager-name manager) name)) (rest (get-manager-reports mngr)))))))]
         [else '()]))]
    (map member->name (list-members name mngr))))

(check-expect (list-direct-reports "Edelgard" MNGR-EX1) (list "Samson" "Serverus"))
(check-expect (list-direct-reports "Edelgard" MNGR-EX2) (list "Lucia" "Gordon" "Edelgard" "Samson" "Serverus"))
(check-expect (list-direct-reports "Astra" MNGR-EX3) '())
(check-expect (list-direct-reports "Astra" MNGR-EX2) '())
(check-expect (list-direct-reports "Astra" (make-manager "Edelgard" (make-team "Customer Service"
                                                                               (list (make-manager "Astra" (make-team "HR" (list (make-ic "Jesse"))))
                                                                                     (make-ic "Lucia")
                                                                                     (make-ic "Gordon")
                                                                                     (make-manager "Astra" (make-team "Writers" (list (make-ic "James"))))))))
              (list "Jesse" "James"))
(check-expect (list-direct-reports "Edelgard" MNGR-EX4) (list "Lucia" "Gordon" "Edelgard" "Sean" "Astra" "Jesse" "Edelgard" "Lucia" "Gordon" "Edelgard" "Samson" "Serverus"))

;;! Problem 3

;; Complete the following function design. Hint: this requires an accumulator

;;! list-managers : String Manager -> [List-of String]
;;! Produces a list of all the managers who directly manage someone with the
;;! given name. When several people have the same name, list all their managers.
(define (list-managers name mngr)
  (local
    [(define (get-manager-reports mngr)
       (filter manager? (team-members (manager-team mngr))))
     (define (member->name member)
       (cond
         [(ic? member) (ic-name member)]
         [else (manager-name member)]))
     (define (name-found? name mngr)
       (ormap (lambda (member) (string=? (member->name member) name)) (team-members (manager-team mngr))))
     (define (list-managers-helper name mngr acc)
       (cond
         [(name-found? name mngr) (cons (manager-name mngr) acc)]
         [else (foldr list-managers-helper
                      acc
                      (make-list (length (get-manager-reports mngr)) name)
                      (get-manager-reports mngr)
                      )]))]
    (list-managers-helper name mngr '())))

(check-expect (list-managers "James" MNGR-EX4) (list "Sean" "Astra"))
(check-expect (list-managers "Astra" MNGR-EX4) (list "Edelgard"))
(check-expect (list-managers "Sean" MNGR-EX3) '())
(check-expect (list-managers "Serverus" MNGR-EX5) (list "Edelgard" "Astra"))
