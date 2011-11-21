; Project Euler problems, 20-29
; keroserene

(load "io.scm")
(load "words.scm")

; ------------ Problem 22 ------------
; "total names score from "names.txt"

(define (p22)
  (define s (sort (txt->words "names.txt") string<?)) ; load and sort
  (define (comb li i acc)
    (if (eq? () li) acc
      (comb (cdr li) (+ 1 i) (+ acc (* i (word->int (car li)))))))
  (comb s 1 0))


