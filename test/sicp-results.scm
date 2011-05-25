; http://community.schemewiki.org/?SICP-Solutions

(define (test a b)
	(if (not (= a b))
			(error "a is not equal b")
			#t))

;; ex 1.2 

(/ (+ 5 
      4 
      (- 2 (- 3 (+ 6 (/ 4 5))))) 
   (* 3 
      (- 6 2) 
      (- 2 7))) 
 


;; Result is -0.24666666666666667, or -37/150


;;  ex1.3: Define a procedure that takes three numbers as arguments  
;;  and returns the sum of the squares of the two larger numbers. 
 
(define (square x) (* x x)) 
 
(define (sum-of-squares x y) (+ (square x) (square y))) 


 
(define (sum-of-squared-largest-two x y z) 
        (cond ((= (min x y z) x) (sum-of-squares y z)) 
              ((= (min x y z) y) (sum-of-squares x z)) 
              ((= (min x y z) z) (sum-of-squares x y)))) 
 
;; Testing


(test  9 (square 3))
(test 41 (sum-of-squares 5 4))
(test 25 (sum-of-squared-largest-two 1 3 4))
(test 25 (sum-of-squared-largest-two 4 1 3))
(test 25 (sum-of-squared-largest-two 3 4 1))
 
 
;; ex 1.3 
;; implemented using only techniques covered to this point 
 
(define (square x) (* x x)) 
 
(define (sum-of-squares x y) 
  (+ (square x) (square y))) 
 
(define (largest-two-of-three x y z) 
  (if (>= x y) 
      (sum-of-squares x (if (>= y z) y z)) 
      (sum-of-squares y (if (>= x z) x z)))) 
 
;; tests 
(test 25 (largest-two-of-three 2 3 4))
(test 25 (largest-two-of-three 4 2 3))
(test 25 (largest-two-of-three 3 4 2))
 
(define (smallest-two-of-three a b c) 
  (if (< a b)  
    (if (< a c) a c) 
    (if (< b c) b c))) 
 
(define (square a) 
  (* a a)) 
 
(define (sum-of-squares-largest-two-of-three a b c)  
  (+ (square a) (square b) (square c) (- (square (smallest-two-of-three a b c))))) 
(define (square a) 
  (* a a)) 
 
(define (two-greatest-first? x y z) 
  (and (>= x z) (>= x y))) 
 
(define (sum-of-greatest-squares x y z)  
  (if (two-greatest-first? x y z) 
    (+ (square x) (square y)) 
    (sum-of-greatest-squares y z x))) 
(define (sum-of-squares x y) 
  (+ (* x x) (* y y))) 
 
(define (sum-of-squares-of-two-largest a b c) 
  (cond ((and (<= a b) (<= a c)) (sum-of-squares b c)) 
        ((<= a b) (sum-of-squares a b)) 
        (else (sum-of-squares a c)))) 
(define (sum-of-squared-largest-two a b c) 
  (- (+ (square a) (square b) (square c)) (square (min a b c)))) 
;; a recursive solution 
 
(define (sum-of-squares-of-two-largest a b c) 
  (define (>= x y) 
    (or (> x y) (= x y))) 
  (define (square x) 
    (* x x)) 
  (cond ((and (>= a b) (>= b c)) (+ (square a) (square b))) ; a >= b >= c 
        ((and (>= b a) (>= c b)) (+ (square b) (square c))) ; a <= b <= c 
        (else (sum-of-squares-of-two-largest b c a)))) 
 

(test 25 (sum-of-squares-of-two-largest 1 3 4))
(test 25 (sum-of-squares-of-two-largest 4 1 3))
(test 25 (sum-of-squares-of-two-largest 3 4 1))
;;  ex1.3: Define a procedure that takes three numbers as arguments  
;;  and returns the sum of the squares of the two larger numbers. 
 
(define (sum-of-squared-largest-two a b c) 
  (+ (if (or (> a b) (> a c)) 
         (* a a ) 
         0) 
     (if (or (> b a) (> b c)) 
         (* b b ) 
         0) 
     (if (or (> c a) (> c b)) 
         (* c c ) 
         0)))

(test 25 (sum-of-squared-largest-two 1 3 4))
(test 25 (sum-of-squared-largest-two 4 1 3))
(test 25 (sum-of-squared-largest-two 3 4 1))
