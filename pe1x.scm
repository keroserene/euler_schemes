; Project Euler problems, 10-19
; keroserene


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



