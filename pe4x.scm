; Project Euler problems, 40-49
; keroserene

; ------------ Problem 45 ------------
; "Next Triangular number that is also pentagonal and hexagonal after 40755 (h_143)"


; T: t(t+1)/2
; P: p(3p-1)/2
; H: h(2h-1)

; Must satisfy: t(t+1)/2 = p(3p-1)/2 = h(2h-1)
;	-> t(t+1) = p(3p-1) = 2h(2h-1)
;	-> t^2 + t = 3p^2 - p = 4h^2 - 2h
; Hexagonal numbers jump by the fastest increments.

; h_0 = 144
(define (p45)
	(define (hx x) (* x (- (* 2 x) 1)))
	(define (px x) (* x (- (* 3 x) 1)) 1/2)
	(define (tx x) (* x (+ x 1)) 1/2)

	; Reversals - for P,
	;	3p^2 - p = 2x
	;	3p^2 - p - 2x = 0
	;	p = (1 +- sqrt(1^2 + 4*3*2x))/(2*3)
	;		where p is an intger.
	(define (pr? x)
		(let ((d (sqrt (+ 1 (* 4 3 2 x)))))
			(integer? (/ (+ 1 d) 6))))

	; For T,
	;	t^2 + t - 2x = 0
	;       t = (-1 +- sqrt(1^2 + 4*1*2x))/2
	;	where t is an integer
 	(define (tr? x)
		(let ((d (sqrt (+ 1 (* 4 2 x)))))
			(integer? (/ (+ 1 d) 2))))
 
	(define (TPH h)
		(let ((x (hx h)))
		(if (and (pr? x) (tr? x)) x
			(TPH (+ 1 h)))))
	(TPH 144))
