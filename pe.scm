; Project Euler problems
; keroserene

; ------------ Problem 1 ------------
; "Sum all the multiples of 3 or 5 below 1000"
; Sums m + 2m + 3m + ... + km such that km < n
(define (sumOf m n)
	(let ((x (quotient n m)))
	(* m (* (+ 1 x) x 1/2))))

; By I&E, sum all multiples of 3 and multiples of 5, subtract multiples of 15
(define (p1) (- (+ (sumOf 3 999) (sumOf 5 999)) (sumOf 15 999)))
	

; ------------ Problem 2 ------------
; "Sum of the even fibonnaci numbers below 4 million"
; Every 3rd fibonnaci is even by properties of addition.
(define (fib3 a b f)
	(if (< (+ a b) 4000000)
		(fib3 (+ a (* 2 b)) (+ (* 2 a) (* 3 b)) (lambda (sum) (f (+ a b sum))))
		(f 0)))

(define (p2) (fib3 1 1 (lambda (ret) ret)))
	

; ------------ Problem 3 ------------
; "Largest prime factor of 6008514751"
(define p3const 600851475143)

; Divide out entire factor
(define (elimFact n d f)
	(if (not (and (> n d) (eq? 0 (modulo n d))))
		(f n)
		(elimFact (/ n d) d f)))

; Jump by 2 and 4
(define (jump2and4 n d f)
	(if (> n d)
		(elimFact n (+ 2 d) (lambda(x) 
		(elimFact x (+ 6 d) (lambda(y) 
		(jump2and4 y (+ 6 d) f)))))
	(f n)))

; Check 2 and 3 manually, then jump 2 and 4
(define (p3) 
	(check p3const 2 (lambda (r2) 
	(check r2 3 (lambda (r3) 
	(check r3 5 (lambda (n) 
	(jump2and4 n 5 (lambda(ret) ret)))))))))


; ------------ Problem 4 ------------
; "Largest palindromic product of two 3-digit numbers"

(define (isPalindrome x)
	(define (reversal x r)
		(if (eq? 0 x) r	
			(reversal (quotient x 10) (+ (* 10 r) (remainder x 10)))))
	(eq? x (reversal x 0)))
	
(define (p4)
	(define (tryMult x y f)
		(if (< x 100) (f 0)
		(if (or (isPalindrome (* x y)) (< y 100))
			(tryMult (- x 1) x (lambda(p) (f (max p (* x y)))))
			(tryMult x (- y 1) f))))
	(tryMult 999 999 (lambda(p) p)))


; ------------ Problem 5 ------------
; "Smallest number evenly divisible by each number between 1 and 20"
; Must eliminate common factors.
(define (p5)
	(define (isDivis n d) (eq? 0 (remainder n d)))
	(define (tryNum n d)
		(if (< d 2) n
		(if (isDivis n d) (tryNum n (- d 1))
			(tryNum (* n (/ d (gcd n d))) (- d 1)))))
	(tryNum 1 20))


; ------------ Problem 6 ------------
; "Difference of sum of squares and square of sums of first 100 naturals"
(define (p6)
	(define (sqOsum n) (square (* n (+ n 1) 1/2)))
	(define (nthOdd n) (- (* 2 n) 1))
	; Sums of n consective odds = n^2, so sum of m consecutive squares
	; must be m*1 + (m-1)*3 + (m-2)*5 + ... (1*(2n+1)
	(define (sumOsq n)
		(define (combine n m) (if (< 0 n)
			(+ (* m (nthOdd n)) (combine (- n 1) (+ 1 m))) n))
		(combine n 1))
        (- (sqOsum 100) (sumOsq 100)))


; ------------ Problem 7 ------------
; "10001st prime"

; Sieve of erasthowhatchamawhosit
; n - Sieve up to the nth prime (except for 2)
(define (soe n)
	(define (elim cnt x f)
		(if (f x) (elim cnt (+ 1 x) f)
		(if (eq? n (+ 1 cnt)) x
			(elim (+ 1 cnt) (+ 2 x)
			(lambda(v) (or (eq? 0 (remainder v x)) (f v)))))))
	(if (< n 2) 2 (elim 1 3 (lambda(x) (eq? 0 (remainder x 2))))))

(define (p7) (soe 10001))
