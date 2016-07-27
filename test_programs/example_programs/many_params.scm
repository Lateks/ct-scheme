(define test
  (lambda (a b c d e f g h i)
    (+ a b c d e g h i)))

(display (test 1 2 3 4 5 6 7 8 9))
(newline)

(define test2 test)

(display (test2 1 2 3 4 5 6 7 8 9))
(newline)
