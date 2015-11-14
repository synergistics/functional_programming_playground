(load "lib/curry.scm")

; NOTE: These combinators may be arbitrarily named 

(define K
  (curry 
    (lambda (a b) a)
    2))

(define S
  (curry
    (lambda (a b c) ((a c) (b c)))
    3))

(define I ((S K) K))

