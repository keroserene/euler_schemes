; Project Euler problems, 20-29
; keroserene

(load "io.scm")
(load "words.scm")
(load "arith.scm")

; ------------ Problem 22 ------------
; "total names score from "names.txt"

(define (p22)
  (define s (sort (txt->words "names.txt") string<?)) ; load and sort
  (define (comb li i acc)
    (if (eq? () li) acc
      (comb (cdr li) (+ 1 i) (+ acc (* i (word->int (car li)))))))
  (comb s 1 0))

   (define (perm mag left acc)
    (if (< mag 0) acc
    (let* ((p (! mag)) (q (quotient left p)))
    (if (eq? 0 q)
      (perm (- mag 1) left acc)
      (perm (- mag 1) (remainder left p) (+ acc (* q (pow 10 mag)))))))) 

; ------------ Problem 24 ------------
; "millionth lexicographical permutation of digits 0 - 9"

(define (p24)
  (define (perm mag left acc)
    (if (< mag 0) acc
    (let* ((p (! mag)) (q (quotient left p)))
    (if (eq? 0 q)
      (perm (- mag 1) left acc)
      (perm (- mag 1) (remainder left p) (+ acc (* q (pow 10 mag))))))))
  (define (make-awesome x)
    (define (next i used r)
      (define (nom c k) 
        (if (vector-ref used k) (nom c (+ 1 k))
	(if (eq? 0 c) k
	  (nom (- c 1) (+ 1 k)))))
      (if (< i 0) r
        (let* ((o (digit x i)) (d (nom o 0)))
	  (vector-set! used d #t)
          (next (- i 1) used (+ r (* d (pow 10 i)))))))
    (next 9 (make-vector 10) 0))
  (make-awesome (perm 9 999999 0)))



; ------------ Problem 27 ------------
; "Find the product of the coefficients, a and b, 
;  for the quadratic expression that produces the maximum number of primes for 
;  consecutive values of n, starting with n = 0."

; Quadratic of form n^2 + an + b = p
; with |a| < 1000, |b| < 1000

; Some math work:
;    (n^2 + an + (b-p) = 0
; ->  n = (-a +/- sqrt(a^2 - 4(b-p)))/2     --> must be integer
; ->  2n = -a +- sqrt(a^2-4b+4p))            --> must be even
;
; So, if a is even, then the sqrt must be even.
;     if a is odd, then the sqrt must be odd.

; Other things to note: for n = 0, the formula reduces to b = p. 
;  ---> Therefore b must be prime!

; Also for n = 1, formula reduces to 1 + a + b, which must also be prime.
; However, then p - b is even, and p - b - 1 is odd, so a must be odd

; List of all primes under 1000 for b, there is a corresponding
; maximum prime sequence given values for a.

(define (p27)
  (define TOTAL 1000)
  (define plist (prime-list total))
  (define pv (intlist->vectorize (prime-list 300000)))
  (define a-range (range -999 999 2))

  ; The formula.
  (define (f n a b) (+ (square n) (* a n) b))

  ; Best a given b ... returns an (i,a) tuple
  (define (a-search b)
    (define (n-iter i sl pa)
;      (println "[" b "] Iteration " i " prev: " prv " possibilities remaining: " (length sl))
      (if (list-empty? sl) ;(begin (println "[" b " * " pa "]=> " i) 
                           (list i pa)
;        ((1) (begin (println "b[ " b "] : i = " i " and a = " (car sl))) (list i (car sl))
        (n-iter (+ 1 i) 
	  (filter (lambda(a) 
	    (let ((p (f i a b))) 
	      (and (> p 1) (> (vector-ref pv p) 0)))) sl) (car sl))))
    (n-iter 1 a-range 0))

  ; Calculate maximum a and b keyed by the largest index
  (let ((al (map (lambda(b) (let ((rs (a-search b))) (list (first rs) (* b (second rs))))) plist)))
    (second (fold-right (lambda(x r) (if (> (first x) (first r)) x r)) (list 0 0) al))))
