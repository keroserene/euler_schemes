; Helper number functions
; keroserene

(load "data.scm")
(load "arith.scm")

; Character -> value
(define (romechar->val c)
  (case c
    ((#\I #\i) 1)
    ((#\V #\v) 5)
    ((#\X #\x) 10)
    ((#\L #\l) 50)
    ((#\C #\c) 100)
    ((#\D #\d) 500)
    ((#\M #\m) 1000)
    (else 0)))

(define (val->romechar v)
  (case v
    ((0) "")
    ((1) "I")
    ((5) "V")
    ((10) "X")
    ((50) "L")
    ((100) "C")
    ((500) "D")
    ((1000) "M")
    (else (let* ((s (pow 10 (- (number-length v) 1)))
                 (f (* 5 s)))
      (if (> (- v f) 0)
        (string-append (val->romechar f) (val->romechar (- v f)))
        (string-append (val->romechar s) (val->romechar (- v s))))))))


(define (compare-rome op a b) (op (romechar->val a) (romechar->val b)))

(define (rome>=? a b) (compare-rome >= (romechar->val a) (romechar->val b)))

; Roman numerals defined as strings
(define (romenum->int r)
  (define ds (string->list r))

  (define (parse s p t a)
    (if (list-empty? s) (+ a t)
    (let* ((c (car s)) (n (cdr s)) (v (romechar->val c)))
      (cond ((eq? p c) (parse n c (+ t v) a))
            ((compare-rome > c p) (parse n c v (- a t))) ;subtraction
	    (else (parse n c v (+ a t)))))))
  (println ds)
  (parse ds #f 0 0))

; Converts a number into roman numeral notation (optimal)
; Int->string
(define (int->romenum x)
  (define l (- (number-length x) 1))
  (define msd (digit x l))
  (define msp (* msd (pow 10 l)))
  (if (eq? 0 x) ""
  ; of subtraction factor
  (if (and (< l 4)
      (or (eq? msd 4) (eq? msd 9)))

      (let* ((sd (pow 10 l))
             (np (+ sd x)))
      (string-append (val->romechar sd) (int->romenum np)))
    (string-append (val->romechar msp) (int->romenum (- x msp))))))
 
