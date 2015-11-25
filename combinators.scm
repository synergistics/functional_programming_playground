(load "lib/curry.scm")

; NOTE: These combinators may not correspond with the ones that actually have these names
; No promises :D

(define K
  (curry 
    (lambda (a b) a)
    2))

(define S
  (curry
    (lambda (a b c) ((a c) (b c)))
    3))

(define I ((S K) K))

