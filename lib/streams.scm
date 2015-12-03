(load "lib/util/require")
(require '("lib/lazy"))

(define-syntax stream
  (syntax-rules ()
    ((stream x y)
     (cons x (defer y)))))

(define (s-head xs)
  (car xs))

(define (s-tail xs)
  (now (cdr xs)))

(define (s-zip f xs1 xs2)
  (stream (f (s-head xs1) (s-head xs2)) (s-zip f (s-tail xs1) (s-tail xs2))))
  
(define (s-filter f xs)
  (if (f (s-head xs))
    (stream (s-head xs) (s-filter f (s-tail xs)))
    (s-filter f (s-tail xs))))

(define (s-map f xs)
  (stream (f (s-head xs)) (s-map f (s-tail xs))))

(define (s-reduce f xs acc)
  (stream acc (s-reduce f (s-tail xs) (f acc (s-head xs)))))

(define (s-nth n xs)
  (if (zero? n)
    xs
    (s-nth (- n 1) (s-tail xs))))

(define (s-take-aux n xs acc)
  (cond 
    ((or (null? xs) (zero? n)) (reverse acc))
    (else (s-take-aux (- n 1) (s-tail xs) (cons (s-head xs) acc)))))

(define (s-take n xs)
  (s-take-aux n xs ()))

(define (s-range x y inc)
  (if (= x y)
    ()
    (stream x (s-range (+ x inc) y inc))))

