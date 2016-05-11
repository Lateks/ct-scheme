(display (car '(1 2 3))) ; 1
(newline)
(display (cdr '(1 2 3))) ; (2 3)
(newline)
(display (cons 1 (cons 2 '()))) ; (1 2)
(newline)
(display (cdr (cons 1 (cons "foo" "bar")))) ; (foo . bar)
(newline)
(display (car "foo")) ; type error
(newline)