; Helper I/O functions
; keroserene

 
(load "arith.scm")

; [Any params out of specified ranges result in undefined behavior]

; Converts a single digit, 0-9 inclusive, to a character representing that digit. 
(define (int->char x) (integer->char (+ x (char-code #\0))))

; Converts a character representing a digit 0-9 inclusive, to the actual integer
(define (char->int x) (- (char-code x) (char-code #\0)))

; Casting integer -> string ie. 123 becomes "123"
(define (int->string n)
  (apply string (map int->char (int->list n))))

; string->int is defined in numbers.scm to avoid circular ref

; Take any arguments and turn it into a string easily
(define (word a . r)
   (apply string-append (map (lambda(x) 
     (cond ((integer? x) (int->string x))
           (else x))) (cons a r))))

; Lexical comparison between two words
(define (before? a b)
  (define (poke al bl)
    (if (eq? () al) a
    (if (eq? () bl) b
    (if (char=? (car al) (car bl))  (poke (cdr al) (cdr bl))
    (if (char<? (car al) (car bl)) a b)))))
  (eq? a (poke (string->list a) (string->list b))))
                                                   
(define (empty-string? s) (string=? "" s))
						   
; Converts a word to the integer value composed of alphabetical sum of characters						   
(define (word->int w)
  (apply + (map (lambda(c) (+ 1 (- (char->integer c) (char->integer #\A)))) 
    (string->list w))))


