; Project Euler problems, 20-29
; keroserene

(load "io.scm")
(load "words.scm")
(load "arith.scm")

; ------------ Problem 22 ------------
; "total names score from "names.txt"

(define (p22)
  (define s (sort (txt->words "names.txt") string<?)) ; load and sort
  (define (comb li i acc)
    (if (eq? () li) acc
      (comb (cdr li) (+ 1 i) (+ acc (* i (word->int (car li)))))))
  (comb s 1 0))

   (define (perm mag left acc)
    (if (< mag 0) acc
    (let* ((p (! mag)) (q (quotient left p)))
    (if (eq? 0 q)
      (perm (- mag 1) left acc)
      (perm (- mag 1) (remainder left p) (+ acc (* q (pow 10 mag)))))))) 

; ------------ Problem 24 ------------
; "millionth lexicographical permutation of digits 0 - 9"

(define (p24)
  (define (perm mag left acc)
    (if (< mag 0) acc
    (let* ((p (! mag)) (q (quotient left p)))
    (if (eq? 0 q)
      (perm (- mag 1) left acc)
      (perm (- mag 1) (remainder left p) (+ acc (* q (pow 10 mag))))))))
  (define (make-awesome x)
    (define (next i used r)
      (define (nom c k) 
        (if (vector-ref used k) (nom c (+ 1 k))
	(if (eq? 0 c) k
	  (nom (- c 1) (+ 1 k)))))
      (if (< i 0) r
        (let* ((o (digit x i)) (d (nom o 0)))
	  (vector-set! used d #t)
          (next (- i 1) used (+ r (* d (pow 10 i)))))))
    (next 9 (make-vector 10) 0))
  (make-awesome (perm 9 999999 0)))
