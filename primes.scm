

(define (lol)
	(if (< 6 5) (+ 5 2)))

(define (seo n)
 	(define P (make-bit-string (- n 2) #t))
	(define (prime? k) (bit-string-ref P (- k 2)))
        (define (nope! k) (bit-string-clear! P (- k 2))) 
	(define (flip! k) 
		(let loop ((i 1))
		(if (< (* k i) n) ((nope! (* k i)) (loop (+ 1 i))))))
	(define (run k)
		(if (prime? k) (flip! k))
		(run (+ k 1)))
	(run 2))

