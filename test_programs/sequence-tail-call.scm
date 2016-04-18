(define say-hello
  (lambda ()
    (display "Hello\n")))

(define test-tc
  (lambda ()
    (and #t 1 (say-hello))))

(test-tc)
