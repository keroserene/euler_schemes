; Playing with math

(define (lg2 x) (/ (log x) (log 2)))


(define roflcopter (+
	(* 10/21 (- 0 (* 7/10 (lg2 7/10)) (* 3/10 (lg2 3/10))))
	(* 11/21 (- 0 (* 5/11 (lg2 5/11)) (* 6/11 (lg2 6/11))))
))


