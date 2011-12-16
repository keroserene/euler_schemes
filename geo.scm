; Helper geometry functions
; keroserene 

; Cartesian slope between two points (x1,y1) (x2,y2)
(define (slope-of x1 y1 x2 y2)
  (/ (- y2 y1) (- x2 x1)))

(define (ortho-to m)
  (/ (- 0 1) m))

(define (manhattan-dist p1 p2)
  (+ (abs (- (first p2) (first p1))) (abs (- (second p2) (second p1)))))

; Returns an equation of form
; y = ax + b
; where b is the y intercept
; and a is the slope
(define (line-equation x1 y1 x2 y2)
  ; b = y-intercept 
  (define a (slope-of x1 y1 x2 y2))
  (define b (- y1 (* a x1 -1)))
  (list a b))

; Return line equation of form y = ax + b in (a b) tuple
; given a point and a slope
(define (point-slope p s)
  (define b (- (second p) (* s (first p))))
  (list s b))

; Returns eq of line l1 reflected across l2
; obtains intersection.
; picks arbitrary point, reflects easily,
; recalculates slope
; y = m1x + b1
; y = m2x + b2
; m1x + b1 = m2x + b2
; (m1 - m2)x = b2 - b1
(define (line-reflect l1 l2)
  (define m1 (first l1))
  (define b1 (second l1))
  (define m2 (first l2))
  (define b2 (second l2))
  (define ort (ortho-to m2))

  (define ix (/ (- b2 b1) (- m1 m2)))
  (define iy (+ (* m1 ix) b1))

  (define ax (+ ix 5))   ; Arbitrary point
  (define ay (+ (* m1 ax) b1)) 

  (define tl (point-slope (list ax ay) ort))
  (define tm (first tl))

  ; tangent intersection point of reflection
  (define tix (/ (- (second tl) b2) (- m2 tm)))
  (define tiy (+ (* m2 tix) b2))
  
  (define rx (- (* 2 tix) ax))   ;reflected point
  (define ry (- (* 2 tiy) ay))

  (point-slope (list ix iy) (slope-of ix iy rx ry)))



; Given a b c, returns the two possible x by quadratic formula.
(define (quadratic a b c)
  (define d (sqrt (- (square b) (* 4 a c))))
  (list 
    (/ (+ (- 0 b) d) (* 2 a))
    (/ (- (- 0 b) d) (* 2 a))))


