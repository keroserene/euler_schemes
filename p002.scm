; Sum of the even fibonnaci numbers below 4 million
; keroserene

; Problem 2

; Every 3rd fibonnaci is even by properties of addition.

(define (fib3 a b f)
	(if (< (+ a b) 4000000)
		(fib3 (+ a (* 2 b)) (+ (* 2 a) (* 3 b)) (lambda (sum) (f (+ a b sum))))
		(f 0)))

(define p2 (fib3 1 1 (lambda (ret) ret)))
	



