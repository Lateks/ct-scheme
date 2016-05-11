(define adder
  (lambda (x)
    (lambda (y)
	  (+ x y))))

(define add5 (adder 5))
(display (add5 10)) ; 15
(newline)
(display ((adder 10) 3)) ; 13
(newline)