(display (null? '())) ; #t
(display (null? (list))) ; #t
(display (null? '(1 2 3))) ; #f
(display (null? (list 1))) ; #f
(display (null? "")) ; #f
(display (null? "Hello")) ; #f
