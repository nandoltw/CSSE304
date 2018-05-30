; CSSE304-01 Yuankai Wang (Kevin) Assignment2
; Problem 1 (a)
(define fact 
	(lambda (n)
	  (if (zero? n)
	  	 1
         (* n (fact (- n 1))))))

; Problem 1 (b)
(define choose
	(lambda (n k)
		(/ (fact n) (* (fact (- n k)) (fact k)))))

; Problem 2
(define range
	(lambda (n m)
		(if (>= n m)
			'()
			(cons n (range (+ n 1) m)))))

; Problem 3
(define set?
	(lambda (list)
		(if (null? list)
			#t
			(if (null? (cdr list))
				#t
				(if (duplicate? (car list) (cdr list))
					#f
					(set? (cdr list)))))))

(define duplicate?
	(lambda (n list)
		(if (equal? n (car list))
			#t
			(if (null? (cdr list))
				#f
				(duplicate? n (cdr list))))))

; Problem 4
(define sum-of-squares
	(lambda (lon)
		(if (null? lon)
			0
			(if (null? (cdr lon))
				(* (car lon) (car lon))
				(+ (* (car lon) (car lon)) (sum-of-squares (cdr lon)))))))

; Problem 5
(define make-vec-from-points
	(lambda (p1 p2)
		(if (null? p1)
			'()
			(cons (- (car p2) (car p1))
				(make-vec-from-points (cdr p1) (cdr p2))))))

; Problem 6
(define dot-product
	(lambda (v1 v2)
		(if (null? v1)
			0
			(+ (* (car v1) (car v2)) (dot-product (cdr v1) (cdr v2))))))

; Problem 7
(define vec-length
	(lambda (v)
			(sqrt (+ (+ (* (car v) (car v)) (* (cadr v) (cadr v))) (* (caddr v) (caddr v))))))

; Problem 8
(define distance
	(lambda (p1 p2)
		(vec-length (make-vec-from-points p1 p2))))

; Problem 9
(define cross-product
	(lambda (v1 v2)
		(list (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2)))
			(- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
			(- (* (car v1) (cadr v2)) (* (cadr v1) (car v2))))))

; Problem 10
(define parallel?
	(lambda (v1 v2)
		(and (and (zero? (car (cross-product v1 v2))) (zero? (cadr (cross-product v1 v2)))) (zero? (caddr (cross-product v1 v2))))))

; Problem 11
(define collinear?
	(lambda (p1 p2 p3)
		(parallel? (list (- (car p1) (car p2)) (- (cadr p1) (cadr p2)) (- (caddr p1) (caddr p2)))
		           (list (- (car p3) (car p2)) (- (cadr p3) (cadr p2)) (- (caddr p3) (caddr p2))))))