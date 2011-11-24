; Helper printing functions
; keroserene

(load "words.scm")

(define (print m . r)
  (display m)
  (if (not (eq? `() r)) (apply print r))
   #t )

; Newline
(define (nl) (display "\n"))

; Printline
(define (println m . r)
  (print m)
  (if (not (eq? `() r)) (apply print r))
  (nl) #t)

; Horizontal Line
(define (hl) (display "---------------------------------------------------------------------------------\n"))
 
