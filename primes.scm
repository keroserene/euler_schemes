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
        ((eq? x 1) 2)
	((eq? x 2) (if s? 5 3))
        (else (begin (fill! 3) (pr 5 (- x 3) #f 5 `(3 2) )))))
                                     
; Sum all primes up til x				     
(define (prime-sum x)
  (prime 1337 x #t #t #f))

; Return a list of primes <= x
(define (prime-list x)
  (prime 1337 x #f #t #t))











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

