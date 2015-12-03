; The code for the article
;
;    From: oleg-at-pobox.com
;    Newsgroups: comp.lang.scheme
;    Subject: Generators in Python, Icon and Scheme
;    Date: 2 Oct 2001 20:38:29 -0700
;    Message-ID: <7eb8ac3e.0110021938.b7d04ca@posting.google.com>
;    NNTP-Posting-Date: 3 Oct 2001 03:38:29 GMT
;
; See enumerators-callcc.html in this directory for detailed explanations.
;
; The code is tested on Gambit-C 3.0, SCM 5d6, Bigloo 2.4b, and Scheme48.
; To run the code, you merely need to 'load' it.
;
; $Id: generators.scm,v 1.2 2003/02/06 18:50:37 oleg Exp oleg $

; procedure: generative GENERATOR
; where GENERATOR is a procedure of one argument: SUSPEND (which is
; also a procedure of one argument).
; The procedure GENERATOR is called to perform the iteration. Whenever
; the procedure is about to yield a value, it should call SUSPEND
; and pass it that value. When SUSPEND returns, the GENERATOR may look
; for another value to yield. The GENERATOR returns when there is
; no more values to generate.
; The result of 'generative' is a lazy list made of all values passed
; to SUSPEND.
; Note, that SUSPEND is an ordinary, first-class value. GENERATOR can
; manipulate that value as any other value. This is in stark
; contrast to Python, where the corresponding 'yield' keyword
; is a _keyword_.

; Lazy list: a promise, which upon forcing returns '() or
; (cons value lazy-list)
; data LazyList a = Promise (Nil | (Cons a (LazyList a)))

(define (generative generator)
  (delay
    (call-with-current-continuation
     (lambda (k-main)
       (define (suspend val)
	 ; (display `("val" ,val) stderr)
	 (call-with-current-continuation
	  (lambda (k-reenter)
	    (k-main
	     (cons val
		(delay
		  (call-with-current-continuation
		   (lambda (k-new-main)
		     (set! k-main k-new-main)
		     (k-reenter #f)))))))))
       (generator suspend)
       (k-main '())))))

; This is the Icon's FIND generator,
; which searches for occurrences of a 'pattern' in a 'str'
(define (find-str-generator pattern str)
  (lambda (suspend)
    (let* ((pat-len (string-length pattern))
	   (str-len (string-length str))
	   (last-pos (- str-len pat-len))); last position where match can occur
      (do ((i 0 (+ 1 i)))
	  ((> i last-pos))		; a naive algorithm
	(if (string=? pattern (substring str i (+ i pat-len)))
	    (suspend i))))))

                ; Some implementations of Scheme can do forcing implicitly...
(define (fcar x) (car (force x)))
(define (fcdr x) (cdr (force x)))
(define (fnull? x) (null? (force x)))

; A lazy filter
(define (lfilter pred lst)
  (delay
    (cond
     ((fnull? lst) lst)
     ((pred (fcar lst)) (cons (fcar lst) (lfilter pred (fcdr lst))))
     (else (force (lfilter pred (fcdr lst)))))))

; A forceful for-each
(define (ffor-each proc lst)
  (if (not (fnull? lst))
      (begin (proc (fcar lst))
	     (ffor-each proc (fcdr lst)))))

; test 1
; An example from Icon
; http://www.cs.arizona.edu/icon/docs/ipd266.htm
;   sentence := "Store it in the neighboring harbor"
;   if (i := find("or", sentence)) > 5 then write(i)
; Note, unlike Icon's indices, our indices are zero-based.

(for-each display
  '("Test1" #\newline
    "Print the index of the first occurrence of \"or\" which is greater than 5"
    #\newline))
(let ((sentence
       "Store it in the neighboring harbor")
      (pattern "or"))
  (display
   (fcar
    (lfilter (lambda (val) (> val 5))
	     (generative (find-str-generator pattern sentence))))))
(newline)


; test 2
; Another example from the Icon documentation: 
; write indices of all occurrences of a pattern in a sentence
;    every write(find("or", sentence))
(for-each display
  '(#\newline "Test2" #\newline
    "Print indices of all occurrences of \"or\" in a sentence" #\newline))
(let ((sentence
       "Store it in the neighboring harbor")
      (pattern "or"))
  (ffor-each (lambda (val) (display val) (display " "))
	     (generative (find-str-generator pattern sentence))))
(newline)
 
; test 3
; From Charming Python : Iterators and simple generators 
; By David Mertz, Ph.D. (mertz@gnosis.cx)
; Autodidact, Gnosis Software, Inc.
; September 2001
; http://www-106.ibm.com/developerworks/linux/library/l-pycon?t=grl,l=252,p=iterators
;   >>>> # A recursive generator that generates Tree leaves in in-order.
;   >>> def inorder(t):
;   ...     if t:
;   ...         for x in inorder(t.left):
;   ...             yield x
;   ...         yield t.label
;   ...         for x in inorder(t.right):
;   ...             yield x


; tree:: '() | (list label tree-left tree-right)

(define (inorder tree)
  (lambda (suspend)
    (if (pair? tree)
	(apply
	 (lambda (label left right)
	   (ffor-each suspend (generative (inorder left)))
	   (suspend label)
	   (ffor-each suspend (generative (inorder right))))
	 tree))))

(define (make-binary-tree depth)
  (let loop ((i 1) (depth depth))
    (if (<= depth 0) '()
	(list i (loop (* 2 i) (- depth 1)) (loop (+ 1 (* 2 i)) (- depth 1)))
	)))

(for-each display
  '(#\newline "Test3" #\newline
    "in-order traversal of a tree" #\newline))
(let ((tree (make-binary-tree 3)))
  (display "Original tree") (newline)
  (display tree) (newline)
  (display "In-order-traversal: ")
  (ffor-each (lambda (val) (display val) (display " "))
	     (generative (inorder tree))))
(newline)
   



