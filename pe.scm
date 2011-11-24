; Management functions for Project Euler
; keroserene

(load "print.scm")

; Run problem n
(define (pe n)
  (define file (string-append "pe" (quotient n 10) "x.scm"))
  (define prob (string-symbol (string-append "p" n)))
  (println "Loading " file "...")
  (prob))
