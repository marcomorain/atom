; Test below my Marc:

(+ 1 2)
;(- 3 4)
;(- (a . b) (c . d))

(if #t 1)
(if #f 1 2)
(if #f 1)

(define assert
	(lambda (c)
		(if (not c)
			(error "assertion failed"))))

(assert #t)

(assert 1)

(assert '())
(assert cons(1 2))
(assert () (error "some error"))

'()

; Set awy to one
(define awy 1)

; Then see what awy is
awy

(cons 1 2)

(define marc (cons 3 4))

marc

(car marc)
(cdr marc)

(define finnegan #f)
finnegan

(define true #t)

(define bools (cons true finnegan))
(car bools)
(cdr bools)

(boolean? #t)
(boolean? #f)
(boolean? 1)
(boolean? marc)

(number? 1)
(number? awy)
(number? #t)
(number? #f)
; bug here  (number? #\s)
(number? #\space)

(pair? marc)
(pair? #t)
(pair? #f)
(pair? #\newline)
(pair? bools)
(pair? awy)


bools
(set-car! bools #f)
(set-cdr! bools #t)
bools

(list? (quote(a b c))) ; => #t
(list? (quote()))	     ; => #t
(list? (quote(a . b))) ; => #f
(list? '(a b c))

(+ 1 1 1 1)
(* 1)
(*)

(> 1 2)
(> 3 2 3)
(> 2 1)
(> 2 1 0)
(> 2 1 3)


(cond ((> 3 3) 'greater)
			((< 3 3) 'less)
			(else 'equal))

(cond ((> 3 2) 'greater)
			((< 3 2) 'less))
									
;(cond ((assv 'b '((a 1) (b 2))) => cadr)
;		(else #f))

