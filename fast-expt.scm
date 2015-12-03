(define (fast-expt-iter x n acc)
  (cond ((= n 0) acc)
        ((even? n) (fast-expt-iter x (/ n 2) (square (if (= acc 1) x acc))))
        (else (fast-expt-iter x (- n 1) (* acc x)))))

(define (fast-expt x n)
  (fast-expt-iter x n 1))
