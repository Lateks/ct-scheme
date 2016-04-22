(define foo
  (lambda (x)
    (display "Hello")
	(define y x)
	(display y)
	(newline)))

(foo)