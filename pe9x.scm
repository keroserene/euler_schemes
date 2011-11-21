; Project Euler problems, 90-99
; keroserene

; ------------ Problem 90 ------------
; Squares and cubes

; Possible squares, 01, 04, 09, 16, 25, 36, 49, 64, 81
; Separate cube constraints: 
; Also, 6 == 9
; Vector, 0 - 9, value 0 = not in a cube, 1 = in first cube, 2 = in second cube, 3 = on both cubes
; Invariant: sum of elements must be 3*6 = 18
; ex. #(1 2 2 2 2 1 1 1 3 3))) describes the dice in the first part
(define (p90)
	; Whether a given dice configuration is valid
	(define (valid? d)
		(define (sumA i acc)
			(if (eq? 10 i) acc
				(sumA (+ 1 i) (+ acc 
					(if (eq? 1 (modulo (vector-ref d i) 2)) 1 0)))))
 		(define (sumB i acc)
			(if (eq? 10 i) acc
				(sumB (+ 1 i) (+ acc 
					(if (> (vector-ref d i) 1) 1 0)))))
		(and (eq? 6 (sumA 0 0)) (eq? 6 (sumB 0 0))))
	; Whether a given valid dice configuration satisfies the squares
	(define (counted? d)
		(define (has? x y)
			(let ((a (vector-ref d x)) (b (vector-ref d y)))
			(and (> a 0) (> b 0)
			(or
				(and (eq? 1 a) (eq? 2 b))
				(and (eq? 2 a) (eq? 1 b))
				(or (eq? 3 a) (eq? b 3))
				))))
			
		(and (valid? d) (has? 0 1) (has? 0 4) 
			(or (has? 0 9) (has? 0 6))
			(or (has? 1 6) (has? 1 9))
			(has? 2 5) 
			(or (has? 3 6) (has? 3 9))
			(or (has? 4 9) (has? 4 6)) 	; 4 9 is redundant with 6 4
			(has? 8 1)))

	(define base (make-vector 10 3))	; Subtract 12 somewhere, minimum 0 per element
	(define (cubage)
		; Permutation iterating
		(define (perm d i c f)
			(if (eq? 0 c) (f (if (counted? d) 1 0))	; try permutation
			(if (or (> i 9) (eq? 0 (vector-ref d 9))) (f 0)
			(let* ((d2 (vector-copy d))
				(i2 (+ i (if (eq? 0 (vector-ref d2 i)) 1 0))))
;				((if (> i2 9) (f s)
			(vector-set! d2 i2 (- (vector-ref d2 i2) 1))
			(if (> i2 9) (f s)
				(perm d2 i2 (- c 1) 
				(lambda(r) (perm d (+ 1 i) c (lambda(t) (f (+ r t)))))))))))
		(perm base 0 12 (lambda(s) s)))

	; Must divide by 2 to remove double counts (dice are not unique)
	(/ (cubage) 2))


; Better way to do it?
(define p90b 0)	