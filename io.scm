; Helper I/O functions
; keroserene

; Returns a list of words from a text file, assuming "WORD1","WORD2",... format
(define (txt->words file)
  (define port (open-input-file file))
   (define (parse cw acc)
    (if (eof-object? (peek-char port)) acc
    (let ((c (read-char port)))
    (if (or (eq? c #\") (eq? c #\,))
      (parse "" (if (eq? 0 (string-length cw)) acc (cons cw acc)))
      (parse (string-append cw (string c)) acc)))))
  (parse "" (list)))

