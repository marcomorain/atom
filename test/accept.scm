(define errors 0)

(define assert
  (lambda (test) (if test
                     
                     errors
                     (set! errors (+ errors 1)))))
(assert (eq? 2 (+ 1 1)))
(display "Errors: ")
(display errors)