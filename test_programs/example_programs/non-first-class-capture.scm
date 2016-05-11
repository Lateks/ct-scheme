(define test
  (lambda (n)
    (define inc
	  (lambda ()
	    (set! n (+ n 1))))
	(inc)
	n))

(display (test 5)) ; 6
(newline)