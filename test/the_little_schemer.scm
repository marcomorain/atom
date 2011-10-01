'()
'(a (a v) d)

(string #\a #\b)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
    
(define expect-true
  (lambda (x)
    (or x (error "expected to be true"))))
    
(define expect-false
  (lambda (x)
    (or (not x) (error "expected to be false"))))
    
(define expect-equal
  (lambda (a b)
    (or (equal? a b) (error "expected a and b to be the same"))))
    
(expect-true  #t)
(expect-false #f)

(expect-true  (atom? 'atom))
(expect-true  (atom? 'turkey))
(expect-true  (atom? 1492))
(expect-true  (atom? 'u))
(expect-true  (atom? '*abc$))
(expect-true  (list? '(atom)))
(expect-true  (list? '((atom turkey) or)))
(expect-true  (list? '(how are you doing so far)))
(expect-true  (list? '(((how) are) ((you) (doing so)) far)))
(expect-true  (list? '()))
(expect-false (atom? '()))
(expect-true  (list? '(() () () ())))
(expect-equal 'a (car '(a b c)))
(expect-equal '(a b c) (car '((a b c) x y z)))
