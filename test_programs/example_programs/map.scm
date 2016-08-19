(define double
  (lambda (x)
    (+ x x)))
	
(display (map double '(1 2 3 4 5)))
(newline)

(display (map (lambda (a b) (+ a b)) '(1 2 3 4 5) '(5 4 3 2 1 0)))
(newline)

(display (map (lambda (a) (+ a 1)) (cons 1 (cons 2 3))))
(newline)
