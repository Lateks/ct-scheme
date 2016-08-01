(define cons2 cons)
(display cons)
(newline)
(display cons2)
(newline)
(display (cons2 1 2)) ; (1 . 2)
(newline)
(define my-newline newline)
(define my-minus -)
(display (my-minus 1)) ; -1
(my-newline)
(display (my-minus 5 2 1)) ; 2
(my-newline)
(display (my-minus)) ; arity mismatch error