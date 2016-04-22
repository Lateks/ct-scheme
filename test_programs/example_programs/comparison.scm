(display (< 1 2)) ; #t
(display (> 1 2)) ; #f
(display (< 1 1)) ; #f
(display (< 1 2 3)) ; #t
(display (< 1 2 1)) ; #f
(display (> 3 2 1)) ; #t
(display (> 3 2 3)) ; #f
(display (< 1 #f)) ; type error
