(define (rev xs)
  (define (rev-list xs acc)
    (if (null? xs)
        acc
        (rev-list (cdr xs) (cons (car xs) acc))))

  (cond ((list? xs) (rev-list xs ()))
        (else xs)))

(define (deep-reverse xs)
  (rev 
    (map (lambda (x)
           (if (list? x) 
             (deep-reverse x)
             x))
         xs)))

