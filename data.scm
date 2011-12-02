; Helper data structure functions
; keroserene

;(load "print.scm")

; Queue
;(define-structure queue head tail)

(define (list-empty? l) (eq? `() l))

(define (list-has? l e)
  (there-exists? l (lambda(x) (eq? x e))))

; True if vector contains element
(define (vector-has? v e)
  (list-has? (vector->list v) e))

; Increment integer of a vector
(define (vector-int++ v i) (vector-set! v i (+ 1 (vector-ref v i))))

; Automatically create list containing numbers within range from a to b to c
; With just a, creates list (0, 1, ... a-1)
; With both a and b, creates list (a, ... , b) inclusive
; With a b and c, creates list (a, a+c, a+2c, ... k) 
;    where k is the largest number satisfying k<=c and (k-a) mod c = 0
(define (range a #!optional b c)
  (if (default-object? b) (begin (set! b (- a 1)) (set! a 0))) ; Default case ... 0 through a
  (if (default-object? c) (set! c 1))  
  (let ((n (+ 1 (- b a))))
  (vector->list (make-initialized-vector 
    (+ (quotient n c) (if (eq? 0 (remainder n c)) 0 1)) (lambda(i) (+ (* i c) a))))))


; Remove duplicates from a list (Does not preserve order)
(define (remove-duplicates li)
  (define (check! sli acc)
    (if (list-empty? sli) acc
      (check! (cdr sli) (if (list-has? acc (car sli)) acc (cons (car sli) acc)))))
  (check! li (list)))
