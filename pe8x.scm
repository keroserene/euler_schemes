; Project Euler problems, 80-89
; keroserene 

(load "io.scm")
(load "print.scm")
(load "numbers.scm")

; ------------ Problem 81 ------------
; "Minimal path from top left to bottom right"

; AWARD: TRINARY TRIUMPH - 5th problem
(define (p81)
  (define M (txt->matrix "matrix.txt"))
  (define rows (length M))
  (define cols (length (car M)))
  (define (mx r c) (list-ref (list-ref M r) c))

  ; Dynamic programming bottom-up iteration vector
  (define dp (make-vector cols 999999999))
  (define (dx i) (if (>= i 0) (vector-ref dp i) 99999999))
  (define (ds! i v) (vector-set! dp i v))

  (define (parse i)
    (define (mxmz j)
      (if (>= j cols) #t
      (begin (ds! j (+ (mx i j) (min (dx (- j 1)) (dx j))))
             (mxmz (+ 1 j)))))
    (if (>= i rows) (dx (- cols 1)) ; Completion
    (begin (mxmz 0)
  ;         (println "row: [" i "] " dp " min: " (apply min (vector->list dp)))
           (parse (+ 1 i)))))
  (ds! 0 0)
  (parse 0))

; ------------ Problem 89 ------------
; "Minimal roman numerals"

; AWARD: EASY AS PI - 7th PROBLEM

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
