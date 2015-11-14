(define (maybe x)
  (list
    (list 'val x)
    (list 'fmap
          (lambda (f)
            (if (null? val)
              (maybe ())
              (maybe (f val)))))))
