
(define mutate-arg
  (lambda (x)
    (define get (lambda () x))
	(define set (lambda (y) (set! x y)))
	(set! x (* x 10))
	(cons get set)))

(define test (mutate-arg 5))
(display ((car test))) ; 50
(newline)
((cdr test) 1000)
(display ((car test)))
(newline)
