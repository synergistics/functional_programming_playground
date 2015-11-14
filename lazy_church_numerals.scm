; Lazy Church numerals
; These are modified and just for fun

(load "lib/curry.scm")

(define-syntax defer 
  (syntax-rules ()
    ((defer x)
     (lambda () x))))

(define (now dx) (dx))

;(define (zero x)
  ;x)

; This is an abstract church numeral representing the application of some function on some value (represented by zero)
(define (church n) 
  (if (zero? n)
    zero
    (defer (church (- n 1)))))

; Just want to note, there's a lot of cool stuff to examine here. Tons to learn!

; This completes an abstract church numeral by applying it to a function and a value
; In this way, completing church numeral 15 with the function +1 and the starting value of 0 would return 15
(define (complete n f acc)
  (if (eqv? n zero)
    acc 
    (complete (now n) f (f acc))))

; How is a church different from a lazy cons and what kind of laziness is it?
; You have to unfold layers of laziness to get to the actual value
; Onion laziness I'll call it
; Wait, could these be implemented with conses?

(define (cons-stream x y)
  (cons x (defer y)))

; I don't think so. church works because there's nothing that it really needs to be consed onto
; But wait! That could be an incorrect assumption. See the following

; Actually correct church numerals

(define zero
  (curry
    (lambda (f x) x)
    2))

(define succ
  (curry
    (lambda (n f x) (f ((n f) x)))
    3))

(define plus
  (curry
    (lambda (m n f x) ((m f) ((n f) x)))
    4))

(define mult
  (curry
    (lambda (m n f x) ((m (n f)) x))
    4))

(define pow
  (curry
    (lambda (m n f x) (((n m) f) x))
    4))

(define (num n)
  (letrec
    ((aux
       (lambda (n acc)
         (if (zero? n)
           acc
           (aux (- n 1) (succ acc))))))
     (aux n zero)))
