; Project Euler problems, 30-39
; keroserene

(load "primes.scm")
(load "arith.scm")
(load "numbers.scm")

; ------------ Problem 31 ------------
; "ways to make a 2e (200p) currency"

; Equivalent to all possible combinations of coins >= 2 with value <= 200
(define (p31)
  (define TOTAL 200)
  (define COINS #(1 2 5 10 20 50 100 200))
  (define NCOINS (vector-length COINS))
  (define CNT (make-vector 200 0))  ; Implicit ones

  (define (mem i) (vector-ref CNT i))
  (define (inc i) 
    (begin (display "--- ") (display i) (display "\n") (vector-set! CNT i (+ (vector-ref CNT i) 1))))

  (define (count i s f)
    (if (>= i NCOINS) (f 0)
    (let ((s2 (+ s (vector-ref COINS i))))
      (if (> s2 TOTAL) (f 0)
       (count (+ 1 i) s (lambda(a) 
       (count i s2 (lambda(b) (f (+ 1 a b))))))))))

  (display "Ways to make ") (display TOTAL) (display " pence: ")
  (count 1 0 (lambda(s) (+ 1 s))))


; ------------ Problem 32 ------------
; "pandigital products"

; AWARD: EASY AS PI - 9th PROBLEM

; For 1-9 pandigital, the products axb=c
; must be such that a is 2 digits and b is 3 digits, and c is 4 digits
; It is impossible to have 3 digits * 3 digits = 3 digits,
; and it is impossible to have 2 * 2 digits = >= 5 digits,
; lastly, the first 2 digits of a and b must have a product < 10 so that c is 4 digits.
; This includes {1 and 2-7}, {2 and 3, 4}
(define (p32)
  (define ds `(1 2 3 4 5 6 7 8 9))
  (define pairs (list->vector (list `(1 2) `(1 3) `(1 4) `(1 5) `(1 6) `(1 7) `(2 3) `(2 4) `(3 2))))
  (define cnt (vector-length pairs))
  (define ANS (list)) ; Avoid duplicates

  ; Two digits * 3 digits
  (define (pp i acc)
    (define (pcheck a b)
      (define an (list->int a))
      (define bn (list->int b))
      (define cn (* an bn))
      (define dn (list->int (append a b (int->list cn))))
      (if (>= cn 10000) 0
      (if (and (pandigital? dn) (not (list-has? ANS cn))) 
        (begin (println "pandigital: " an " * " bn "=" cn) (set! ANS (cons cn ANS)) cn) 0)))

    (if (>= i cnt) acc
    (pp (+ 1 i) (+ acc 
       ; Prepare permutations of the digits
       (let* ((pr (vector-ref pairs i)) (a1 (first pr)) (a2 (second pr))
           (dgs (filter (lambda(x) (not (or (eq? a1 x) (eq? a2 x)))) ds))
	   (perms (filter (lambda(e) 
                    (not (or (eq? (first e) (first (second e)))
		             (eq? (first e) (second (second e)))
		             (eq? (second (second e)) (first (second e))))))
                     (cross dgs (cross dgs dgs)))))
           ; Map the sum of valid pandigital products
         (println "ppin " i " with " a1 " and " a2)
         (apply + (map (lambda(e) 
	     (+ (pcheck (list a2 (first e)) (cons a1 (second e)))
	        (pcheck (list a2 (first e)) (cons a1 (second e)))
                (pcheck (list (first e)) (cons a1 (cons a2 (second e))))
                (pcheck (list (first e)) (cons a2 (cons a1 (second e))))
                (pcheck (list a1) (cons a2 (cons (first e) (second e))))
                (pcheck (list a1) (cons (first e) (cons a2 (second e))))
                (pcheck (list a2) (cons a1 (cons (first e) (second e))))
                (pcheck (list a2) (cons (first e) (cons a1 (second e))))
		))
              perms)))))))

  (pp 0 0))


; ------------ Problem 35 ------------
; "Circular primes under 1 million"

; AWARD: EASY AS PI - 6th PROBLEM

(define (p35)
  (define TOTAL 1000000)
  (println "Generating Primes...")
  (define pl (prime-list TOTAL))
  (println "Vectorizing...")
  (define ps (intlist->vectorize pl))
  (define mxps (vector-length ps))
  (define (count spl acc)
    (define (test-circular x)
      (define nl (number-length x))
      (define (nxt? y) 
;        (print " " y "~ ")
        (if (>= y mxps) 0
        (let ((v (vector-ref ps y)))
          (if (eq? x y) 1
;	    (begin (println x " is circulargh") 1)
          (if (> v 0) (nxt? (rotate-number y nl)) 0)))))
      (nxt? (rotate-number x nl)))

    (if (list-empty? spl) acc
      (count (cdr spl) (+ acc (test-circular (car spl))))))
  (println "Counting circular primes...")
  (count pl 0))


; ------------ Problem 38 ------------
; "largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?"

; AWARD: EASY AS PI - 10th PROBLEM

; Given max of 9 digits, n <= 9
; Guaranteed largest pandigital number must be 987654321, but n = 1 here
; Since n > 1, let's try 2, 3, and 4
; Want something which begins with a 9. Will start permutations from greatest number and decrease from there,
; so the first occurence of a solution is the largest!
(define (p38)
  (define digits `(9 8 7 6 5 4 3 2 1))

  ; Multiply base x by i = {1, 2, ... max} such that
  ; the concatenation is exactly 9 digits (0 otherwise)
  (define (mlt x acc cnt i)
    (cond 
      ((> cnt 9) 0)
      ((eq? cnt 9) (let ((fin (list->int acc))) (if (pandigital? fin) (begin (println "Found pandigital: " x " up to i=" i ": " fin) fin) 0)))
      (else (let ((prod (int->list (* x i))))
              (mlt x (append acc (int->list (* x i))) (+ cnt (length prod)) (+ 1 i))))))

  (define (search-max lst)
    (if (list-empty? lst) 123456789
    (let ((v (mlt (list->int (car lst)) (list) 0 1)))
      (if (eq? 0 v) (search-max (cdr lst))
      v))))

  ; 2 digit numbers, starting from 98
  ; 3 digit numbers
  (define l2 (filter (lambda(e) (not (eq? (first e) (second e)))) (cross digits digits)))
  (define l3 (filter (lambda(e) (not (or (eq? (first e) (second e)) (eq? (first e) (third e)))))
                 (map (lambda(el) (cons (first el) (second el))) (cross digits l2))))
  (define l4 (filter (lambda(e) (not (or (eq? (first e) (second e)) (eq? (first e) (third e)) (eq? (first e) (fourth e)))))
                (map (lambda(el) (cons (first el) (second el))) (cross digits l3))))

  (max (search-max l2)
       (search-max l3)
       (search-max l4)))





