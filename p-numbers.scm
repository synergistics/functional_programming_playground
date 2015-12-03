; IT'S FINALLY HAPPENING!! P-NUMBERS IN SCHEME!! WOO!!

(load "lib/util/require")
(require '("lib/streams"))

(define (base num) (car num))

(define (val num) (cdr num))

; Yea, yea, not DRY, just makes semantics easier to follow
(define (p base val) (cons base val))
(define (n base val) (cons base val))
(define (ni val) (cons (find-p-base val) val))

(define (find-p-base num)
  (floor (/ (log num) (log 2))))

(define (pnum->num-aux pnum acc)
  (cond ((zero? acc) (pnum->num-aux pnum (* (expt 2 (base pnum)) 1.0)))
        ((zero? (string-length (val pnum))) (ni acc))
        (else (pnum->num-aux (cons (- (base pnum) 1) (substring (val pnum) 1 (string-length (val pnum))))
                             (cond ((string=? "0" (substring (val pnum) 0 1)) (- acc (* (expt 2 (base pnum)) 1.0)))
                                   ((string=? "1" (substring (val pnum) 0 1)) (+ acc (* (expt 2 (base pnum)) 1.0))))))))

(define (pnum->num pnum)
  (pnum->num-aux pnum 0))

(define (num->pnum-aux num acc)
  (let ((acc-val (val (pnum->num (p (base num) acc)))))
    (cond ((= acc-val (val num)) (p (base num) acc))
          ((> acc-val (val num)) (num->pnum-aux num (string-append acc "0")))
          ((< acc-val (val num)) (num->pnum-aux num (string-append acc "1"))))))


(define (num->pnum num)
  (num->pnum-aux num ""))

; Streams now!

(define (thing x y)
  (s-map ni (s-range x y 1)))

(define (p-thing x y)
  (s-map num->pnum (thing x y)))
