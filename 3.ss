; CSSE304-01 Yuankai Wang (Kevin) Assignment3
; Procedures from Assignment2
(define make-vec-from-points
	(lambda (p1 p2)
		(if (null? p1)
			'()
			(cons (- (car p2) (car p1))
				(make-vec-from-points (cdr p1) (cdr p2))))))

(define vec-length
	(lambda (v)
			(sqrt (+ (+ (* (car v) (car v)) (* (cadr v) (cadr v))) (* (caddr v) (caddr v))))))

(define distance
	(lambda (p1 p2)
		(vec-length (make-vec-from-points p1 p2))))

(define duplicate?
	(lambda (n list)
		(if (null? list)
			#f
			(if (equal? n (car list))
				#t
				(if (null? (cdr list))
					#f
					(duplicate? n (cdr list)))))))

; Problem 1 
(define nearest-point
	(lambda (p list-of-points)
		(nearest-point-has-min p list-of-points (car list-of-points))))

(define nearest-point-has-min
	(lambda (p list-of-points min)
		(if (null? list-of-points)
			min
			(if (< (distance p (car list-of-points)) (distance p min))
				(nearest-point-has-min p (cdr list-of-points) (car list-of-points))
				(nearest-point-has-min p (cdr list-of-points) min)))))

; Problem 2
(define union
	(lambda (s1 s2)
		(if (null? s2)
			s1
			(if (duplicate? (car s2) s1)
				(union s1 (cdr s2))
				(union (cons (car s2) s1) (cdr s2))))))

; Problem 3
(define intersection
	(lambda (s1 s2)
	    (intersection-with-new-set s1 s2 '())))

(define intersection-with-new-set
	(lambda (s1 s2 output)
		(if (null? s2)
			output
			(if (duplicate? (car s2) s1)
				(intersection-with-new-set s1 (cdr s2) (cons (car s2) output))
				(intersection-with-new-set s1 (cdr s2) output)))))

; Problem 4
(define subset?
	(lambda (s1 s2)
		(if (null? s1)
			#t
			(if (duplicate? (car s1) s2)
				(subset? (cdr s1) s2)
				#f))))

; Problem 5
(define relation?
	(lambda (set)
		(if (list? set)
			(if (null? set)
				#t
				(if (duplicate-list (car set) (cdr set))
					#f
					(if (list? (car set))
						(if (equal? (length (car set)) 2)
							(relation? (cdr set))
							#f)
						#f)))
			#f)))

(define duplicate-list
	(lambda (list set)
		(if (null? set)
			#f
			(if (equal? list (car set))
				#t
				(duplicate-list list (cdr set))))))

; Problem 6
(define domain
	(lambda (r)
		(domain-with-output r '())))

(define domain-with-output
	(lambda (r output)
		(if (or (null? r) (null? (cdr r)))
			output
			(if (duplicate? (caar r) output)
				(domain-with-output (cdr r) output)
				(domain-with-output (cdr r) (cons (caar r) output))))))

; Problem 7
(define reflexive?
	(lambda (set)
		(reflexive-require set (union (domain set) (range set)))))

(define reflexive-require
	(lambda (set dom)
		(if (null? dom)
			#t
			(if (duplicate-list (list (car dom) (car dom)) set)
				(reflexive-require set (cdr dom))
				#f))))

(define range
	(lambda (r)
		(range-with-output r '())))

(define range-with-output
	(lambda (r output)
		(if (or (null? r) (null? (cdr r)))
			output
			(if (duplicate? (car (cdr (car r))) output)
				(range-with-output (cdr r) output)
				(range-with-output (cdr r) (cons (car (cdr (car r))) output))))))

; Problem 8
(define hailstone-step-count
	(lambda (n)
		(hailstone-step-count-count n 0)))

(define hailstone-step-count-count
	(lambda (n count)
		(if (equal? n 1)
			count
			(if (equal? (mod n 2) 0)
				(hailstone-step-count-count (/ n 2) (+ count 1))
				(hailstone-step-count-count (+ (* n 3) 1) (+ count 1))))))