; Trying some sorts
; keroserene

; Simple selection sort algorithm with continuation
; "Selects the minimum from the current sublist each time and places it in front"
; l - List to sort
(define (selectionSort l)

	; Helper function - selects an element from a list,
	; and returns the list without that element.
	; sL - current sublist
	; x - element to search for
	; fn - success continuation
	; Invariant: x must be within sL
	(define (select sL x fn)
		(if (eq? (car sL) x)		; Check first element vs the target
			(fn (cdr sL))		; Success!
			; Otherwise, more continuation!
			(select (cdr sL) x (lambda (rest) (fn (cons (car sL) rest))))))
	(define m (apply min l))

        (if (eq? () (cdr l)) l
		; Recurse on each sublist
		(select l m (lambda (rest) (cons m (selectionSort rest))))))

; Simple insertion sort algorithm
; "Takes the first element each time and swaps it through until the minimum location"
; l - List to sort
(define (insertionSort l)
	l)
