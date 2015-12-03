(define-syntax defer 
  (syntax-rules ()
    ((defer x)
     (lambda () x))))

(define (now dx) (dx))
