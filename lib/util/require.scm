(define (require files)
  (for-each 
    (lambda (file) (load file))
    files))
