(display (not 1)) ; #f
(display (not "foo")) ; #f
(display (not #t)) ; #f
(display (not #f)) ; #t
(display (eq? 1 1)) ; #t
(display (eq? 2 1)) ; #f
(display (eq? '(1 2 3) '(1 2 3))) ; #t
(display (eq? (cons 1 2) (cons 1 2))) ; #t
(display (eq? (cons 1 2) (list 1 2))) ; #f
