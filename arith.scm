; Helper math functions
; keroserene

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
