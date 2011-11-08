; Find the sum of all the multiples of 3 or 5 below 1000
; keroserene

; Problem 1

; Sums m + 2m + 3m + ... + km such that km < n
(define (sumOf m n)
	(let ((x (quotient n m)))
	(* m (* (+ 1 x) x 1/2))))

; Sum all multiples of 3 and multiples of 5, subtract multiples of 15
(define p1 (- (+ (sumOf 3 999) (sumOf 5 999)) (sumOf 15 999)))
	



