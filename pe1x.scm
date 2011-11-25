; Project Euler problems, 10-19
; keroserene

(load "print.scm")
(load "data.scm")
(load "arith.scm")

; ------------ Problem 11 ------------
; "Greatest product of 4 adjacent numbers (vertically/horizontally/diagonally)"

(define (p11)
  (define A (list 
   08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
   49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
   81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
   52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
   22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
   24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
   32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
   67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
   24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
   21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
   78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
   16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
   86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
   19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
   04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
   88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
   04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
   20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
   20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
   01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48))

  (define DIM 20) ; 20x20 grid
  (define L 4) ; Lines of 4 numbers
  
  ; Traversing indices. -1 if off the grid
  (define (right i d) (if (>= (modulo i DIM) (- DIM d)) -1 (+ i d)))
  (define (down i d) (if (>= (quotient i DIM) (- DIM d)) -1 (+ (* DIM d) i)))

  ; Diagonals - south east vs south west
  (define (dse i d) 
    (if (or (>= (modulo i DIM) (- DIM d)) 
            (>= (quotient i DIM) (- DIM d))) -1
	(+ i (* d (+ DIM 1)))))
  (define (dsw i d) 
    (if (or (< (modulo i DIM) d)
            (>= (quotient i DIM) (- DIM d))) -1
	(+ i (* d (- DIM 1)))))

  (define (g4 dir i) 
    (if (eq? (dir i 3) -1) -1)
    
;    (print (list-ref A i) " " (list-ref A (dir i 1)) " " (list-ref A (dir i 2)) " " (list-ref A (dir i 3)) "\n")

    (* (list-ref A i) 
       (list-ref A (dir i 1))
       (list-ref A (dir i 2))
       (list-ref A (dir i 3))))

  (define (mR r)
    (apply max (map (lambda (i) (g4 right (+ (* DIM r) i))) (range (- DIM L)))))

  (define (mC r)
    (apply max (map (lambda (i) (g4 down i)) (range (- DIM L)))))

  ; Diagonals indexed from 0 to 2*DIM - 2*L, where 0 is the lowest bottom-left, DIM-L is the central diagonal, and 2*DI - 2*L is the upper right
  (define (mDr d)
    (define base (+ (max 0 d) (* DIM (abs (min 0 d)))))
    (apply max (map (lambda (i) (g4 dse (dse base i)))
      (range (- 17 (abs d))))))

  (define (mDl d)
    (define base (+ (- DIM 1 (max 0 d)) (* DIM (abs (min 0 d)))))
    (apply max (map (lambda (i) (g4 dsw (dsw base i)))
      (range (- 17 (abs d))))))

  ; MAXIMIZE IT! 
  (max (apply max (map (lambda (c) (mC c)) (range DIM)))
       (apply max (map (lambda (r) (mR r)) (range DIM))) 
       (apply max (map (lambda (d) (mDr d)) (range -16 16)))
       (apply max (map (lambda (d) (mDl d)) (range -16 16)))))
  

; ------------ Problem 17 ------------
; How many letters required to list numbers from 1 to 1000
(define (p17)
  (define (tens n)
    (case n
      ((2) 6)  ;twenty
      ((3) 6)  ;thirty
      ((4) 5)   ;forty
      ((5) 5)   ;fifty
      ((6) 5)   ;sixty
      ((7) 7)   ;seventy
      ((8) 6)   ;eighty
      ((9) 6)   ;ninety
      (else 0)))
  (define (teens n)
    (case n
      ((0) 0)    ; (nothing)
      ((1) 3)    ;one
      ((2) 3)    ;two
      ((3) 5)    ;three
      ((4) 4)    ;four
      ((5) 4)    ;five
      ((6) 3)    ;six
      ((7) 5)    ;seven
      ((8) 5)    ;eight
      ((9) 4)    ;nine
      ((10) 3)  ;ten
      ((11) 6)  ;eleven
      ((12) 6)  ;twelve
      ((13) 8)  ;thirteen
      ((14) 8)  ;fourteen
      ((15) 7)    ;fifteen
      ((16) 7)  ;sixteen
      ((17) 9)  ;seventeen
      ((18) 8)   ;eighteen
      ((19) 8)  ;nineteen
      (else (teens (modulo n 10)))))

  ; Sum of the first 10 numbers
  (define (f10)
    (define (fn x f)
      (if (>= x 9)
        (f (teens 9))
        (fn (+ 1 x) (lambda(s) (f (+ (teens x) s))))))
    (fn 1 (lambda(s) s)))

  ; Letters in any number < 100
  (define (letters n) (+ (tens (quotient n 10)) (teens n)))

  (define (count100)      ; first 100 numbers
    (define (lc x f)
      (if (eq? x 100)
        (f 0)
        (lc (+ 1 x) (lambda(s) (f (+ (letters x) s))))))
    (lc 1 (lambda(s) s)))

  (define hundreds (* 7 100 9))
  (define ands (* 3 99 9))
  (define thousand 11)  ;one thousand

  (+ (* 10 (count100))  (* 100 (f10)) hundreds ands thousand))


; ------------ Problem 18 ------------
; "max sum on triangle"

(define (p18)
  (define T #( 
  75
  95 64
  17 47 82
  18 35 87 10
  20 04 82 47 65
  19 01 23 75 03 34
  88 02 77 73 07 63 67
  99 65 04 28 06 16 70 92
  41 41 26 56 83 40 80 70 33
  41 48 72 33 47 32 37 16 94 29
  53 71 44 65 25 43 91 52 97 51 14
  70 11 33 28 77 73 17 78 39 68 17 57
  91 71 52 38 17 14 91 43 58 50 27 29 48
  63 66 04 68 89 53 67 30 73 16 69 87 40 31
  04 62 98 27 23 09 70 98 73 93 38 53 60 04 23))
  (define ROWS 15)
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
