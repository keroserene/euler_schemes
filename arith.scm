; Helper math functions
; keroserene

(load "primes.scm")

; Generic exponent function
; Returns x raised to p
(define (pow x p)
  (define (rec acc n)
    (if (eq? p n) acc
    (rec (* x acc) (+ 1 n))))
  (rec 1 0))

; Returns the nth digit of x (right to left, 0-indexed)
; Returns 0 if n is invalid index for x
; Requires: n >= 1
(define (digit x n)
  (if (< n 0) 0
  (modulo (quotient x (pow 10 n)) 10)))

; Turns an integer into a list of its digits, left to right
(define (int->list x)
  (define (f x acc) 
    (if (eq? 0 x) acc
        (f (quotient x 10) (cons (digit x 0) acc))))
  (cond ((eq? 0 x) `(0))
        (else (f x `()))))

; Factorial
(define (! n)
  (define (rec i acc) 
    (if (<= i 1) acc (rec (- i 1) (* acc i))))
  (rec n 1))

; Binomial coefficient
(define (binom n k)
  (if (< n k) 0 (/ (! n) (* (! k) (! (- n k))))))


; Arithmetic sum m + 2m + 3m + ... + km such that km < n
(define (asum m n)
  (let ((x (quotient n m)))
  (* m (* (+ 1 x) x 1/2))))
     

; Determine is a number is a palindrome, by checking if reverse is equal
(define (palindrome? x)
  (define (reversal x r)
    (if (eq? 0 x) r  
      (reversal (quotient x 10) (+ (* 10 r) (remainder x 10)))))
  (eq? x (reversal x 0)))
 
; nth triangular number
(define (tri n) (* n (+ 1 n) 1/2))

(define (tri? n) 
  (integer? (/ (- (sqrt (+ 1 (* 4 2 n)))) 1) 2))

(define (ltri n) (floor->exact (/ (- (sqrt (+ 1 (* 8 n))) 1) 2)))
 

; Prime factorize! (returns a list of (prime, exponent) tuples)
(define (factors x)
  (define (fout p n)
    (prime p)
  (fout 1 x))

 ; Summable of two squares?
(define (squaresum2? x)
  (define (check i)
    (and (<= (square i) (/ x 2))
      (or (integer? (sqrt (- x (square i))))
          (check (+ 1 i)))))
  (check 1))
