; Helper number functions
; keroserene

(load "data.scm")

; Character -> value
(define (romechar->val c)
  (case c
    ((#\I #\i) 1)
    ((#\V #\v) 5)
    ((#\X #\x) 10)
    ((#\L #\l) 50)
    ((#\C #\c) 100)
    ((#\D #\d) 500)
    ((#\M #\m) 1000)
    (else 0)))

(define (compare-rome op a b) (op (romechar->val a) (romechar->val b)))

(define (rome>=? a b) (compare-rome >= (romechar->val a) (romechar->val b)))

; Roman numerals defined as strings
(define (romenum->integer r)
  (define ds (string->list r))

  (define (parse s p t a)
    (if (list-empty? s) (+ a t)
    (let* ((c (car s)) (n (cdr s)) (v (romechar->val c)))
      (cond ((eq? p c) (parse n c (+ t v) a))
            ((compare-rome > c p) (parse n c v (- a t))) ;subtraction
	    (else (parse n c v (+ a t)))))))

;           (nt (if (or (eq? 0 t) (eq? p c)) (begin (println "OMG") (+ t (romechar->val c))) 0))
 ;          (na (+ a (if (compare-rome < c p) t 0))))
	   ; (begin (println "WTF") (- 0 t)) 0))))
	            ;(if (compare-rome < c p) t 0)))) ; Subtraction?
;'      (println "LOL -> " c nt na)     
 ;     (parse (cdr s) c nt na))))

  (println ds)
  (parse ds #f 0 0))
 
