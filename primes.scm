; Helper math functions - PRIMES!!!

(load "print.scm")

; sieve of erasthothenes

; Returns the x-th prime.
; cap - Maximum capacity of number. (Smaller for faster sieves)
; sum? - Whether to take a sum of all the primes or not #t or #f
; lst? - return list of primes?
(define (prime x #!optional cap sum? til? lst?)
  ; Only counting 1 mod 4 numbers starting with 3, 5, 7, 11, 13, 17, 19, 23, 29
  (define N (if (eq? #!default cap) 
    (begin (println "Defaulting to max prime of 100000.") 100000) 
    (max 0 (- (quotient cap 2) 1))) )
  (define s? (if (eq? #t sum?) #t #f))  ; FLAGS
  (define tc? (if (eq? #t til?) #t #f))
  (define ls? (if (eq? #t lst?) #t #f))

  (define A (make-bit-string N #f))  ; The sieve
  (define (p->o p) (- (quotient p 2) 1))  ; Converts from prime to index
  (define (p? p) (not (bit-string-ref A (p->o p)))) ; Limited primality test

  ; Filter out the sieve for prime p (odd offsets are p as well)
  (define (fill! p)
    (define (s! i) (if (< i N) (begin (bit-string-set! A i) (s! (+ i p))) #t))
    (s! (p->o p)))
  
  ; Check next prime number, either collect sum or list
  (define (pr x k four? sum acc)
    (define nx (+ x (if four? 4 2)))
    (if (and tc? (>= x cap))  ; Exceed number bound... done
       (if s? sum
       (if ls? acc))
    (if (eq? 0 k) x            ; Exceed count bound... done
;       (if s? (+ sum x) x))
;       (if s? sum
;       (if ls? acc x)))

    (if (not (p? x)) (pr nx k (not four?) sum acc)  ; Not prime -> continue!
    (begin (fill! x) ; PRIME!
      (pr nx (- k (if tc? 0 1)) (not four?) (+ sum x) (cons x acc)))))))
 
  (cond ((< x 0) (print "n MUST be greater than 0") #f)
        ((and (not tc?) (eq? x 1)) 2)
	((and (not tc?) (eq? x 2)) (if s? 5 3))
	((and tc? (<= cap 2)) (if lst? `() 0))
	((and tc? (<= cap 3)) (if lst? `(2) 2))
	((and tc? (<= cap 5)) (if lst? `(3 2) 5))
        (else (begin (fill! 3) (pr 5 (- x 3) #f 5 `(3 2) )))))
                                     
; Sum all primes up til x				     
(define (prime-sum x)
  (prime 1337 x #t #t #f))

; Return a list of primes <= x
(define (prime-list x)
  (prime 1337 x #f #t #t))


; Prime factorization - returns a list of (p,e) tuples
(define (prime-factorize n)
  (define v (list->vector (reverse (prime-list (+ 1 (quotient n 2)))))) ;(inexact->exact (floor (sqrt n))))))
  (define len (vector-length v))
  (define (dout x p e acc)
    (if (>= p len) (if (<= x 1) acc (cons (list x 1) acc))
    (let* ((pr (vector-ref v p)) (d (/ x pr)))
  ;    (println "factorstep " x " / " pr " gives " d " (with prev exponent " e ")")
      (if (integer? d) (dout d p (+ 1 e) acc) ; Next power
      (dout x (if (< d 1) len (+ 1 p)) 0 (if (> e 0) (cons (list pr e) acc) acc))))))
  (dout n 0 0 (list)))



; Sieve of erasthowhatchamawhosit for primes using continuations
; This one is actually really slow... :(
; n - Sieve up to the nth prime (except for 2)
(define (prime-soe n)
  (define (elim cnt x f)
    (if (f x) (elim cnt (+ 2 x) f)
    (if (eq? n (+ 1 cnt)) x
      (elim (+ 1 cnt) (+ 2 x)
      (lambda(v) (or (eq? 0 (remainder v x)) (f v)))))))
  (if (< n 2) 2 (elim 1 3 (lambda(x) (eq? 0 (remainder x 2))))))
                                                                
;(define (seo n)
; 	(define P (make-bit-string (- n 2) #t))
;	(define (prime? k) (bit-string-ref P (- k 2)))
;        (define (nope! k) (bit-string-clear! P (- k 2))) 
;	(define (flip! k) 
;		(let loop ((i 1))
;		(if (< (* k i) n) ((nope! (* k i)) (loop (+ 1 i))))))
;	(define (run k)
;		(if (prime? k) (flip! k))
;		(run (+ k 1)))
;	(run 2))

