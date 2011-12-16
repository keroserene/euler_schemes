; Pr;oject Euler problems, 140-149
; keroserene

(load "geo.scm")
(load "print.scm")

; ------------ Problem 144 ------------
; White cell light beam reflections

; represent line equations as (y = ax + b)
; via tuple `(a b)

(define (p144)

  (define start `(0.0 10.1))
  (define impact1 `(1.4 -9.6))

   ; 4x^2 + y^2 = 100
  (define (y-of x +?) (* (if +? 1 -1) (sqrt (- 100 (* 4 (square x))))))

  ; Line-Ellipse intersection. Given a line, returns (x,1)
  ; 4x^2 + y^2 = 100
  ; y = mx + b
  ; => 4x^2 + (mx+b)^2 = 100
  ; => 4x^2 + m^2x^2 + 2mbx + b^2 = 100
  ; => (4+m^2)x^2 + (2mb)x + (b^2-100) = 0
  ; Quadratic eq
  ; => x = (-(2mb) +- sqrt((2mb)^2 - 4(4+m^2)(b^2-100)))/)2(4+m^2)) 
  (define (lei line)
    (define m (first line))
    (define d (second line))

    (define a (+ 4 (square m)))
    (define b (* m d 2))
    (define c (- (square d) 100))

    (define xsol (quadratic a b c))
;    (println "isec m=" m " yicpt=" d "   " a " " b " " c)
;    (println (first xsol) ",     " (second xsol))
    (map (lambda(x) (list x (+ (* m x) d))) xsol))

  (define (tangent-at x y) (/ (* x -4) y))

  ; Returns an eq
  (define (reflect line s)
    (define x (first s))
    (define y (second s))
    (define t (tangent-at x y)) ;tangent
    (define p (ortho-to t)) ; perpendicular slope
    (define pl (point-slope s p)) 
    (line-reflect line pl))

  (define (bounce p1 line i)
    (define li (lei line))
    ; Correct bounce intercept
    (define s (if (< (manhattan-dist (first li) p1) (manhattan-dist (second li) p1)) 
                     (second li) (first li)))

    (println "Bounce " i ": " s)
    (if (and (< (abs (first s)) 0.01) (> (second s) 0)) i
      (bounce s (reflect line s) (+ 1 i))))
 
  (println impact1)
  (bounce start (apply line-equation (append start impact1)) 0))
