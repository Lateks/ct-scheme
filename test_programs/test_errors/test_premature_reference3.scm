(define test
  (lambda ()
    (define inc (lambda () (+ x 1)))
	(define y (inc))
    (define x 1)
	y))

(display (test))
