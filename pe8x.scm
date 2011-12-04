; Project Euler problems, 80-89
; keroserene 

(load "io.scm")
(load "print.scm")
(load "numbers.scm")


; ------------ Problem 89 ------------
; "Minimal roman numerals"

(define (p89)
  (define a (map symbol->string (txt->numbers "roman.txt")))
  (define (opt s r)
    (if (list-empty? s) r
    (let* ((c (car s))
          (v (romenum->int c))
	  (b (int->romenum v))
          (d (- (string-length c) (string-length b))))
    (opt (cdr s) (+ r d)))))
  (opt a 0))
