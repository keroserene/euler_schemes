; Project Euler problems, 30-39
; keroserene

(load "primes.scm")
(load "arith.scm")

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
    (begin (display "rofling ") (display i) (display "\n") (vector-set! CNT i (+ (vector-ref CNT i) 1))))

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

; For 1-9 pandigital, the products axb=c
; must be such that a is 2 digits and b is 3 digits, and c is 4 digits
; It is impossible to have 3 digits * 3 digits = 3 digits,
; and it is impossible to have 2 * 2 digits = >= 5 digits,
; lastly, the first 2 digits of a and b must have a product < 10 so that c is 4 digits.
; This includes {1 and 2-7}, {2 and 3, 4}
(define (p32)
  (define ds #(1 2 3 4 5 6 7 8 9))
  (define abpairs #(`(1 2) `(1 3) `(1 4) `(1 5) `(1 6) `(1 7)

  (define (check a b c)
    (

  (check (





; ------------ Problem 35 ------------
; "Circular primes under 1 million"

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
