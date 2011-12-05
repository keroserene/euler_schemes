; Project Euler problems, 70-19
; keroserene 

(load "io.scm")
(load "arith.scm")

; ------------ Problem 70 ------------
; "smallest totient(n) which is a permutation of digits of n" 



; ------------ Problem 79 ------------
; Shortest possible secret passcode

; Algorithm: scan list and identify first-digits only
(define (p79)
  (define a (list->vector (map int->list (txt->numbers "keylog.txt"))))
  (define len (vector-length a))

  (define (cut! s)

    ; Remove all first elements from the numbers in the vector
    (define (purge! li i)
      (if (>= i len) #t
      (let ((cc (vector-ref a i)))
        (if (and (not (list-empty? cc)) (list-has? li (car cc)))
	   (vector-set! a i (cdr cc)))
        (purge! li (+ 1 i)))))

    ; Obtain set of digits which can be in the front
    (define (runthrough i y n)

      ; Ensure any digits that are not in the frot f the current int lit are otin the final list of "firsts"
      (define (rid! rc)
        (if (list-empty? rc) #t
	(let ((cc (car rc)))
          (if (not (list-has? n cc)) (set! n (cons cc n)))
          (if (list-has? y cc) (set! y (delete! cc y)))
	  (rid! (cdr rc)))))

      (if (>= i len) y ; Done
      (let ((c (vector-ref a i))) ; Current code
        (if (list-empty? c) (runthrough (+ 1 i) y n)
        (let ((ca (car c)))
          (rid! (cdr c))
          (runthrough (+ 1 i)
            (if (and (not (list-has? y ca)) (not (list-has? n ca))) (cons ca y) y)
	  n))))))

    (define nxt (runthrough 0 (list) (list)))
    (if (list-empty? nxt) s
      (begin 
        (purge! nxt 0)
        (cut! (append s nxt)))))

  (println "Shortest possible keycode: ")
  (list->int (cut! (list))))
