; Project Euler problems, 90-99
; keroserene

; ------------ Problem 90 ------------
; Squares and cubes

; Possible squares, 01, 04, 09, 16, 25, 36, 49, 64, 81
; Separate cube constraints: 
; Also, 6 == 9
; Vector, 0 - 9, value 0 = not in a cube, 1 = in first cube, 2 = in second cube, 3 = on both cubes
; Invariant: sum of elements must be 3*6 = 18
(define (p90)
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
	(define (counted? d)
		(define (has? x y) 
			(or
			(and (eq? 1 (modulo (vector-ref d x) 2)) (> (vector-ref d y) 1))
			(and (> (vector-ref d x) 1) (eq? 1 (modulo (vector-ref d y) 2)))))
		(and (has? 0 1) (has? 0 4) 
			(or (has? 0 9) (has? 0 6))
			(or (has? 1 6) (has? 1 9))
			(has? 2 5) 
			(or (has? 3 6) (has? 3 9))
			(or (has? 4 9) (has? 4 6)) 
			(or (has? 6 4) (has? 9 4))
			(has? 8 1)))
	(define (cubage d acc)
		(define (next d) d)
			
		(if (eq? 0 (vector-ref d 0)) acc ; end search
			(cubage
				(next d)
				(+ acc (if (counted? d) 1 0)))))
	
;	(define base #(1 1 1 1 1 1 0 0 1 0)) ; Distribute +11 among the vector!
	(define base (make-vector 10 3))	; Subtract 12 somewhere, minimum 0 per element
;	(cubage #(3 3 3 3 3 3 0 0 0 0) 0)
	(counted? #(1 2 2 2 2 1 1 1 3 3)))
