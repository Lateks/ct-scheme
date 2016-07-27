(define test
  (lambda (a b c d e f g h i)
    (+ a b c d e g h i)))

(display (test 1 2 3 4 5 6 7 8 9))
(newline)

(define test2 test)

(display (test2 1 2 3 4 5 6 7 8 9))
(newline)

(define test3
  (lambda ()
    (lambda (a b c d e f) (+ a b c d e f))))

(display ((test3) 1 2 3 4 5 6))
(newline)

(define test4
  (lambda (n)
    (lambda (a b c d e f) (+ a b c d e f n))))
	
(display ((test4 10) 1 2 3 4 5 6))
(newline)
