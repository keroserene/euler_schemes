; Project Euler problems, 360-369
; keroserene


(load "arith.scm")

; ------------ Problem 360 ------------
;"sum of manhattan distances of integer coordinates on sphere"

; Can boil down to 1/3 of 1/8 of the surface of the sphere- ie. 1/48 slice
; Additional optimization: (waring's problem)
; "A positive integer can be represented as a sum of two squares precisely if its prime factorization contains no odd powers of primes of the form 4k + 3."

(define (p360)
  (define r (pow 10 4))
 
  (define (edge? a b c) (or (eq? 0 a) (eq? b c) (eq? a b)))

  ; a < b < c - (edges are double counted - so they have a single multiplier, whereas areas have a double multiplier.)
  (define (cnt a b acc)
    (define c (sqrt (- (square r) (square a) (square b))))
    (cond ((> a c) acc) ; DONE!
          ((> b c); ((not (squaresum2? (- (square r) (square a)))))
               (begin (println "Slice " a ": " acc " so far...") (cnt (+ a 1) (+ a 1) acc))) ; Next row
	  ((integer? c) (cnt a (+ b 1)
	      (+ acc (* (if (edge? a b c) 1 2) (+ a b c)))))
	  (else (cnt a (+ b 1) acc))))
    
  (+ (* 24 (cnt 0 1 0)) 
     (* 6 r))) ; Ortho corners
  


 
 
