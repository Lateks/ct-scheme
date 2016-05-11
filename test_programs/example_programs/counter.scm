(define setup-counter
  (lambda ()
    (define c 0)
	(define get (lambda () c))
	(define increment (lambda () (set! c (+ c 1))))
	(define increment-and-get
	  (lambda ()
	    (increment)
		(get)))
	(set! c 1)
	(list get increment increment-and-get)))

(define ctr (setup-counter))
(define getter (car ctr))
(define inc (car (cdr ctr)))
(define comb (car (cdr (cdr ctr))))

(display (getter)) ; 1
(newline)
(inc)
(inc)
(inc)
(display (getter)) ; 4
(newline)
(display (comb)) ; 5
(newline)