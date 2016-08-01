(display "Expecting: ")
(display (+ 1 2 3 4 5 6 7 8 9))
(newline)

(define test
  (lambda (a b c d e f g h i)
    (+ a b c d e f g h i)))

(display "Result #1: ")
(display (test 1 2 3 4 5 6 7 8 9))
(newline)

(define test2 test)

(display "Result #2: ")
(display (test2 1 2 3 4 5 6 7 8 9))
(newline)

(display "Expecting: ")
(display (+ 1 2 3 4 5 6))
(newline)

(define test3
  (lambda ()
    (lambda (a b c d e f) (+ a b c d e f))))

(display "Result: ")
(display ((test3) 1 2 3 4 5 6))
(newline)

(display "Expecting: ")
(display (+ 1 2 3 4 5 6 10))
(newline)

(define test4
  (lambda (n)
    (lambda (a b c d e f) (+ a b c d e f n))))

(display "Result: ")
(display ((test4 10) 1 2 3 4 5 6))
(newline)
