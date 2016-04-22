(define x 0)

(define test
  (lambda ()
    (define y (+ x 1))
    (define x 1)
	y))

(display (test))
