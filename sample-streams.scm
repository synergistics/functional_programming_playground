(load "lib/util/require")
(require '("lib/streams"))

(define ones (stream 1 ones))

(define (nats n) (stream n (nats (+ n 1))))

(define fibs (stream 0 (stream 1 (s-zip + (s-tail fibs) fibs))))

(define (e n) (stream (expt (+ 1.0 (/ 1.0 n)) n) (e (+ n 1))))
