(define say-hello
  (lambda ()
    (display "Hello\n")))

(define test-tc
  (lambda ()
    (and #t 1 (say-hello))))

(test-tc)

(define c 0)
(define test-undefined
  (lambda ()
    (or #f (set! c 42))))

(if (test-undefined)
    (display "Got undefined value")
	(display "Don't know what happened"))
(newline)
