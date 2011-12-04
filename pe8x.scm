; Project Euler problems, 80-89
; keroserene 

(load "io.scm")
(load "print.scm")

; ------------ Problem 89 ------------
; "Minimal roman numerals"

(define (p89)
  (define a (map symbol->string (txt->numbers "roman.txt")))
  (println (length a))
  (println a)
  (string? (car a)))
