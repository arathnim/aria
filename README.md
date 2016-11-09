# aria
generalized library to desugar function application

converts `(take 10 map (lambda (x) * x 2) range 100)` into `(take 10 (map (lambda (x) (* x 2)) (range 100)))`