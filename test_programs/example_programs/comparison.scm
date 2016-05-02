(display (< 1 2)) ; #t
(newline)
(display (> 1 2)) ; #f
(newline)
(display (< 1 1)) ; #f
(newline)
(display (< 1 2 3)) ; #t
(newline)
(display (< 1 2 1)) ; #f
(newline)
(display (> 3 2 1)) ; #t
(newline)
(display (> 3 2 3)) ; #f
(newline)
(display (< 1 #f)) ; type error
