; Trying some sorts
; keroserene

; Simple selection sort algorithm with continuation
; l - List to sort
; f - Comparison function
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


