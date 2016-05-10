(define test
  (lambda (n)
	(define my-n n)
    (define inc
	  (lambda ()
	    (set! my-n (+ my-n 1))))
	(inc)
	my-n))

(display (test 5))
(newline)