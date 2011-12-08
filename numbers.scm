; Helper number functions
; keroserene

(load "data.scm")
(load "arith.scm")

; ----------- DIGITS -------------

; Returns the nth digit of x (right to left, 0-indexed)
; Returns 0 if n is invalid index for x
; Requires: n >= 1
(define (digit x n)
  (if (< n 0) 0
  (modulo (quotient x (pow 10 n)) 10)))

(define (number-length x) 
  (define (dout x i)
    (if (eq? 0 x) i (dout (quotient x 10) (+ 1 i))))
  (dout x 0)) 

; eg. xyz -> yzx
(define (rotate-number x #!optional len) 
  (define ofs (if (eq? #!default len) (pow 10 (number-length x)) (pow 10 len)))
  (define ts (* x 10))
  (+ (modulo ts ofs) (quotient ts ofs)))

; Turns an integer into a list of its digits, left to right
(define (int->list x)
  (define (f x acc) 
    (if (eq? 0 x) acc
        (f (quotient x 10) (cons (digit x 0) acc))))
  (cond ((eq? 0 x) `(0))
        (else (f x `()))))
 
; Turns a list of digits into an integer
(define (list->int l)
  (define (parse sl i acc)
    (if (eq? `() sl) acc
    (parse (cdr sl) (- i 1) (+ acc (* (car sl) (pow 10 i))))))
  (parse l (- (length l) 1) 0))

(define (pandigital? x)
  (define bits (make-bit-string 9 #f))
  (define expected #*111111111)

  (define (run v)
    (if (list-empty? v) (bit-string=? bits expected)
    (if (eq? 0 (car v)) #f
    (if (bit-string-ref bits (- (car v) 1)) #f
    (begin (bit-string-set! bits (- (car v) 1)) (run (cdr v)))))))
  (run (int->list x)))

(define (append-ints a b)
  (+ b (* a (pow 10 (number-length b)))))


; ----------- ROMAN NUMERALS -------------

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
    (else (let* ((s (min 1000 (pow 10 (- (number-length v) 1))))
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
  (if (and (< l 3)
      (or (eq? msd 4) (eq? msd 9)))

      (let* ((sd (pow 10 l))
             (np (+ sd x)))
      (string-append (val->romechar sd) (int->romenum np)))
    (string-append (val->romechar msp) (int->romenum (- x msp))))))
 
