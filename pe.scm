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

 ; ------------ Problem 8 ------------
; "Greatest product of 5 consecutive digits of the following constant"
(define p8const 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)

(define (p8) 
	(define (pop1 x) (modulo x 10))

	; Determine largest 5-prod in contiguous non-zero region
	; x - current number stream
	; a - accum
	; c - digits
	; f - continuation upon finding 0
	(define (max5 x a c f)
		(if (eq? 0 (pop1 x)) 
			(if (>= c 10000) (f x a) (f x 0))
		(if (>= c 10000)
			(max5 (quotient x 10) 
				(* (/ a (quotient c 10000)) (pop1 x)) 	; a
				(+ (* 10 (modulo c 10000)) (pop1 x)) 	; c
				(lambda(x p) (f x (max a p))))
			(max5 (quotient x 10) (* a (pop1 x)) (+ (* 10 c) (pop1 x)) f))))

	(define (search x f)
		(if (eq? 0 x) (f 0 0)
		(if (eq? 0 (pop1 x))
			(search (quotient x 10) f)
			(max5 x 1 0 (lambda (y p) (search y (lambda(z q) (f z (max p q)))))))))

	(search p8const (lambda(x p) (max p 0))))


; ------------ Problem 9 ------------
;"Given a + b + c = 1000 and a^2 + b^2 = c^2, find the product abc."

(define (ispt a b c) (eq? (square c) (+ (square a) (square b))))
 	(define (tt a b c)
		(if (or (< a 1) (< c b)) (let ((d (- 1001 c))) 
			(tt (quotient d 2) (- d (quotient d 2)) (- c 1)))
		(if (ispt a b c) (* a b c)
			(tt (- a 1) (+ b 1) c))))
(define (p9) (tt 1 1 998))






