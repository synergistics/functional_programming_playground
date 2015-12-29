(define test-list (list 0 1 2 3 4 5))

(define (foldl f acc xs)
  (cond ((null? xs) acc)
        (else (foldl f (f (car xs) acc) (cdr xs)))))

(define (foldr f end xs)
  (cond ((null? xs) end)
        (else (f (car xs) (foldr f end (cdr xs))))))

(define (foldy-map f xs)
  (foldr (lambda (x ys) (cons (f x) ys)) () xs))

(define (foldy-filter p xs)
  (foldr (lambda (x ys)
           (if (p x)
             (cons x ys)
             (foldy-filter p ys)))
         ()
         xs))

(define (foldy-append xs ys)
  (foldr cons ys xs))

(define (foldy-length xs)
  (foldl (lambda (x ys) (+ 1 ys)) 0 xs))

