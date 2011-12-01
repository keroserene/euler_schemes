; Project Euler problems, 60-69
; keroserene
 
(load "io.scm")
(load "arith.scm")

; ------------ Problem 65 ------------
; "Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e."

(define (p65)
  (define (kth k) (if (eq? 1 (modulo k 3)) (* 2 (+ 1 (quotient k 3))) 1))

  (define (kcoe k) 
    (define (nc i f)
      (if (>= i k) (f 0)
        (nc (+ 1 i) (lambda (q) (f (/ 1 (+ (kth i) q)))))))
    (nc 0 (lambda (r) (+ 2 r))))
	
  (apply + (int->list (numerator (kcoe 99)))))



; ------------ Problem 67 ------------
; "largest sum from top to bottom of a huge triangle"

; Note: it's just a bigger version of problem 18
 
(define (p67)
  (define T (list->vector (txt->numbers "triangle.txt")))
  (define ROWS (ltri (vector-length T)))
  (define last (tri (- ROWS 1)))

  ; Left above, right above
  (define (la i) (let* ((r (ltri i)) (l (tri r))) 
     (if (eq? l i) 0 (vector-ref T (+ (tri (- r 1)) (- i l 1))))))
  (define (ra i) (let* ((r (ltri i)) (l (tri r))) 
     (if (eq? (+ l r) i) 0 (vector-ref T (+ (tri (- r 1)) (- i l))))))
 
  (define (pa! i) 
    (vector-set! T i (+ (vector-ref T i) (max (la i) (ra i)))))

  (define (mx r)
    (define l (tri r))
    (if (eq? ROWS r) (apply max (vector->list (subvector T last (+ last ROWS))))
    (begin 
      (map pa! (range l (+ l r)))
      (mx (+ 1 r)))))

  (mx 0))
