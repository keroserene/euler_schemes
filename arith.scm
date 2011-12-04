; Helper math functions
; keroserene

(load "data.scm")

; Generic exponent function
; Returns x raised to p
(define (pow x p)
  (define (rec acc n)
    (if (eq? p n) acc
    (rec (* x acc) (+ 1 n))))
  (if (< p 0) (/ 1 (pow x (- 0 p)))
      (rec 1 0)))

(define (reciprocal x) (/ 1 x))

; Returns the nth digit of x (right to left, 0-indexed)
; Returns 0 if n is invalid index for x
; Requires: n >= 1
(define (digit x n)
  (if (< n 0) 0
  (modulo (quotient x (pow 10 n)) 10)))

(define (number-length x) 
  (define (dout x i)
    (if (eq? 0 x) i (dout (quotient x 10) (+ 1 i))))
  (dout x 0))

; eg. xyz -> yzx
(define (rotate-number x #!optional len) 
  (define ofs (if (eq? #!default len) (pow 10 (number-length x)) (pow 10 len)))
  (define ts (* x 10))
  (+ (modulo ts ofs) (quotient ts ofs)))


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

; Returns a vector with max(list) indices, and integer counters for number of occurences in the list
; Integer list must contain only naturals (n >= 0)
(define (intlist->vectorize l)
  (define m (apply max l))
  (define v (make-vector (+ 1 m) 0))
  (define (run! sl)
    (if (list-empty? sl) v
    (begin (vector-int++ v (car sl))
    (run! (cdr sl)))))
  (run! l))

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
 
; Summable of two squares? (SLOW)... try the primality tester version
(define (squaresum2? x)
  (define (check i)
    (and (<= (square i) (/ x 2))
      (or (integer? (sqrt (- x (square i))))
          (check (+ 1 i)))))
  (check 1))

; Permutations of a list
(define (permutations-of l #!optional total)
  (define len (length l))
  (define xtra (if (eq? #!default total) 0 (- total len)))

  (define v (sort l <))
  (define (cd sl c d acc)
    (if (list-empty? sl) (* acc (! d))
    (if (eq? (car sl) c) (cd (cdr sl) c (+ 1 d) acc)
    (cd (cdr sl) (car sl) 1 (* acc (! d))))))

  (/ (! (+ len xtra)) (cd v #f 0 1) (! xtra)))

; Totient/divisiblity
(define (coprime? a b) (eq? 1 (gcd a b)))

(define (totient n) 0)

(define (cototient n) (- n (totient n)))
