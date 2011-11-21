; Helper I/O functions
; keroserene
 
; Lexical comparison between two words
(define (before? a b)
  (define (poke al bl)
    (if (eq? () al) a
    (if (eq? () bl) b
    (if (char=? (car al) (car bl))  (poke (cdr al) (cdr bl))
    (if (char<? (car al) (car bl)) a b)))))
  (eq? a (poke (string->list a) (string->list b))))
                                                   
						   
; Converts a word to the integer value composed of alphabetical sum of characters						   
(define (word->int w)
  (apply + (map (lambda(c) (+ 1 (- (char->integer c) (char->integer #\A)))) 
    (string->list w))))
