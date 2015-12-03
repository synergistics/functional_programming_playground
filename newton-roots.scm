(define (average x y) (/ (+ x y) 2.0))

; Returns true if a specific tranformation on x is close to y
; E.g. is the x squared close to y
(define (close-enough? x y f precision)
  (if (<= (abs (- (f x) y)) precision)
    #t
    #f))

(define (newton-root-aux guess x f improve)
  (if (close-enough? guess x f 0.0001)
    guess
    (newton-root-aux (improve guess x) x f improve)))

(define (newton-root x f improve)
  (newton-root-aux  x f improve))

(define (make-root-func nth)
  (lambda (x) 
    (newton-root x 
                 (lambda (x) (expt x nth)) 
                 (lambda (guess x) (average guess (/ x (expt guess (- nth 1))))))))

(define root-1 (make-root-func 1))
(define root-2 (make-root-func 2))
(define root-3 (make-root-func 3))

;Takes a while, really inefficient
(define root-4 (make-root-func 4))

