(define test
  (lambda (test-name test expected)
	(define perform-test
	  (lambda ()
		(define got (test))
		(if (eq? got expected)
			(display "OK\n")
			(begin (display "FAILED: expected ")
		       (display expected)
			   (display " but got ")
			   (display got)
			   (newline)))))
    (display test-name)
	(display " ... ")
	(perform-test)
))

(test "arithmetic #1"
      (lambda () (+ 1 2 3))
      6)

(test "arithmetic #2"
      (lambda () (- 1))
      -1)

(test "arithmetic #3"
      (lambda () (- 1 2 3))
      -4)

(test "arithmetic #4"
      (lambda () (+ 5 (- 7 4)))
      8)

(test "arithmetic #5"
      (lambda () (/ (+ 5 3) (- 3 1)))
      4)

(test "arithmetic #6"
      (lambda () (* 2 4))
      8)
