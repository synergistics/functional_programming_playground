(load "lib/util/require")
(require '("lib/streams"
           "lib/lazy"))

(define (T-idea plus times)
  (lambda (p q) 
    (lambda (a b) (cons (plus (times b q) (times a q) (times a p))
                        (plus (times b p) (times a q))))))

(define T-val->val (cdr (T-idea + *)))

(define T-val->pair (T-idea + *)) 

(define T-pair->pair (T-idea 
                 (lambda (xs1 xs2) (cons (map-pairs + xs1 xs2)))
                 (lambda (xs x) (cons (* (car xs) x) (* (cdr xs) x)))))

(define (fib-idea transformation accessor thing n)
  (cond ((= n 0) thing)
        (else (fib-idea 
                transformation 
                accessor
                (apply transformation (accessor thing))
                (- n 1)))))

(define (fib-pair transformation pair n)
  (fib-idea transformation (lambda (xs) (list (car xs) (cdr xs))) pair n))

(define (fib-val tranformation val n)
  (fib-idea transformation (lambda (x) (list x)) val n))

(define (base-fib-pair n)
  (fib-pair (T-val->pair 0 1) (cons 0 1) n))

(define (calc-step-aux p q n)
  (cond ((equal? (helper 1 p q) (base-fib n)) n)
        (else (calc-step-aux p q (+ n 1)))))  

(define (calc-step p q)
  (calc-step-aux p q 1))

(define (s-weird-fib n p q)
  (stream (fib-val (T-val->val p q) n 1) (s-weird-fib (+ n 1) p q)))

;(map-pairs - (helper 2 p q) (helper 1 p q)))
