(define (flatten xs)
  (cond ((null? xs) ())
        ((not (list? (car xs))) (cons (car xs) (flatten-aux (cdr xs))))
        (else (append (flatten-aux (car xs)) (flatten-aux (cdr xs))))))
  

