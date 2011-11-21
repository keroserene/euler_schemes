; Project Euler problems, 40-49
; keroserene
 
(load "arith.scm")
(load "words.scm")

; ------------ Problem 40 ------------
; "concatenating the naturals, d1 + d10 + d100 + ... + d1000000"
(define (p40)
   (define (d n) ; nth digit
    (define (chop mag off)
      (let* (
        (lo (pow 10 (- mag 1)))
        (hi (pow 10 mag))
        (range (* mag (- hi lo))))
      (if (< off range) 
        (digit (+ lo (quotient off mag)) (- mag 1 (modulo off mag)))
        (chop (+ mag 1) (- off range)))))
    (if (< n 10) n (chop 2 (- n 10)))) 
  (* (d 1) (d 10) (d 100) (d 1000) (d 10000) (d 100000) (d 1000000)))

; ------------ Problem 42 ------------
; "triangle words in words.txt"
; All caps locked.
(define (p42)
  ; triangular number: t(t+1)/2
  ; 2x = t^2 + t -> t^2 + t - 2x = 0
  ; x = (-1 +- sqrt(1^2 + 4*1*2x))/2
  ; True if integral.
  (define (tw? w) 
    (and (> (string-length w) 0)
    (integer? (/ (- (sqrt (+ 1 (* 4 2 (word->int w)))) 1) 2))))

  (define (scan file)
    (define port (open-input-file file))
    (define (count cw acc)
      (if (eof-object? (peek-char port)) acc
      (let ((c (read-char port)))
      (if (or (eq? c #\") (eq? c #\,))
        (count "" (+ acc (if (tw? cw) 1 0)))
        (count (string-append cw (string c)) acc)))))
    (count "" 0))
  (scan "words.txt"))
 


; ------------ Problem 45 ------------
; "Next Triangular number that is also pentagonal and hexagonal after 40755 (h_143)"

; T: t(t+1)/2
; P: p(3p-1)/2
; H: h(2h-1)

; Must satisfy: t(t+1)/2 = p(3p-1)/2 = h(2h-1)
;  -> t(t+1) = p(3p-1) = 2h(2h-1)
;  -> t^2 + t = 3p^2 - p = 4h^2 - 2h
; Hexagonal numbers jump by the fastest increments.

; h_0 = 144
(define (p45)
  (define (hx x) (* x (- (* 2 x) 1)))
  (define (px x) (* x (- (* 3 x) 1)) 1/2)
  (define (tx x) (* x (+ x 1)) 1/2)

  ; Reversals - for P,
  ;  3p^2 - p = 2x
  ;  3p^2 - p - 2x = 0
  ;  p = (1 +- sqrt(1^2 + 4*3*2x))/(2*3)
  ;    where p is an intger.
  (define (pr? x)
    (let ((d (sqrt (+ 1 (* 4 3 2 x)))))
      (integer? (/ (+ 1 d) 6))))

  ; For T,
  ;  t^2 + t - 2x = 0
  ;       t = (-1 +- sqrt(1^2 + 4*1*2x))/2
  ;  where t is an integer
   (define (tr? x)
    (let ((d (sqrt (+ 1 (* 4 2 x)))))
      (integer? (/ (+ 1 d) 2))))
 
  (define (TPH h)
    (let ((x (hx h)))
    (if (and (pr? x) (tr? x)) x
      (TPH (+ 1 h)))))
  (TPH 144))
