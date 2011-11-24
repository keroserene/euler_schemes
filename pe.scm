; Management functions for Project Euler
; keroserene

(load "print.scm")

; Automatically parse/run problem n of Project Euler
(define (pe n)
  (define file (word "pe" (quotient n 10) "x.scm"))
  (define prob (string->symbol (word "p" n)))
  (println "Loading " file "...")
  (load file)
  (println "Attempting problem #" n "... ")
  (prob))
