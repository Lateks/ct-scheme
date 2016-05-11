(display (not 1)) ; #f
(newline)
(display (not "foo")) ; #f
(newline)
(display (not #t)) ; #f
(newline)
(display (not #f)) ; #t
(newline)
(display (eq? 1 1)) ; #t
(newline)
(display (eq? 2 1)) ; #f
(newline)
(display (eq? '(1 2 3) '(1 2 3))) ; #t
(newline)
(display (eq? (cons 1 2) (cons 1 2))) ; #t
(newline)
(display (eq? (cons 1 2) (list 1 2))) ; #f
(newline)
