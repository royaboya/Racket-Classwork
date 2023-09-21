#lang racket


(define-struct planet (name radius rings))
;; A planet is a (make-planet String Number Number) where
;; name is the name of the planet
;; radius is the radius of the planet in miles
;; rings is the number of rings on the planet 

(define P1 (make-planet "Earth" 3958.8 0))
(define P2 (make-planet "Mercury" 1516 0))
(define P3 (make-planet "RandomPlanet" 1176 0))

(define (planet-template p)
((planet-name p) (planet-radius p) (planet-rings p)))

(define (ring-discovered p) 
((make-planet (planet-name p) 
              (planet-radius p)
              (+(planet-rings p)1))))

(define (bigger-planet p1 p2)
    (cond
    [((> (planet-radius p1) (planet-radius p2))) (planet-name p1)]
    [((< (planet-radius p1) (planet-radius p2)))]
    [else (string-append "Tied:" (planet-name p1) " " (planet-name p2) )]
    )
    
)


(define plan1 (make-planet "t" 1 0))
(define plan2 (make-planet "t" 1 0))
(bigger-planet plan1 plan2)




