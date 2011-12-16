; Project Euler problems, 240-249
; keroserene 

(load "data.scm")

; ------------ Problem 243 ------------
; Resilience: "Smallest d such that R(d) < 15499/94744"
(define (p243)
  (define const (/ 15499 94744))

  (define (res d)
    (/ (fold-right (lambda(n r) (+ r (if (eq? 1 (gcd n d)) 1 0))) 0 (range 1 (- d 1)))) (- d 1))

  (define (search d)
    (if (< (res d) const) d
    (search (+ 1 d))))

  (search 2))
