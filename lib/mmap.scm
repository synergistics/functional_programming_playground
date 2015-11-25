(load "lib/defer.scm")
(load "lib/now.scm")
(load "lib/curry.scm")

; Weird because deferred values are our Maybes
; Distorted because there is no Maybe really, just mmap
; Just messing around here
; And it's all lazy for some reason 

(define (mmap predicate f . xs)
  (if (predicate (now x))
    (defer ())
    (defer (apply f (map now xs)))))
    
(define (null-mmap f x)
  (mmap null? f x))


