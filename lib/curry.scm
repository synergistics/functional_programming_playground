(define (curry f n)
  (letrec
    ((aux
       (lambda (f n acc)
         (if (= (length acc) n)
           (apply f (reverse acc))
           (lambda (x) (aux f n (cons x acc)))))))
    (aux f n ())))  
