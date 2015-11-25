(define-syntax defer 
  (syntax-rules ()
    ((defer x)
     (lambda () x))))

