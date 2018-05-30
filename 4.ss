; CSSE304-01 Yuankai Wang (Kevin) Assignment4
; Problem 1 
(define multi-set?
	(lambda (obj)
		(if (list? obj)
			(if (null? obj)
				#t
				(if (all-lists obj)
					(if (equal? (length (car obj)) 2)
						(if (number? (cadr (car obj)))
							(if (> (cadr (car obj)) 0)
								(if (number? (caar obj))
									#f
									(if (duplicate-in-set (caar obj) (cdr obj))
										#f
										(multi-set? (cdr obj))))
								#f)
							#f)
						#f)
					#f))
			#f)))

(define duplicate-in-set
	(lambda (n set)
		(if (null? set)
			#f
			(if (equal? n (caar set))
				#t
				(duplicate-in-set n (cdr set))))))

(define all-lists
	(lambda (set)
		(if (null? set)
			#t
			(if (list? (car set))
				(all-lists (cdr set))
				#f))))

; Problem 2
(define ms-size
	(lambda (ms)
		(ms-size-counter ms 0)))

(define ms-size-counter
	(lambda (ms counter)
		(if (null? ms)
			counter
			(ms-size-counter (cdr ms) (+ counter (cadar ms))))))

; Problem 3
(define matrix-ref
	(lambda (m row col)
		(list-ref (list-ref m row) col)))

; Problem 4
(define matrix?
	(lambda (obj)
		(if (list? obj)
			(if (list? (car obj))
				(if (null? (car obj))
					#f
					(equal-list-length obj (length (car obj))))
				#f)
			#f)))

(define equal-list-length
	(lambda (set l)
		(if (null? set)
			#t
			(if (equal? (length (car set)) l)
				(equal-list-length (cdr set) l)
				#f))))

; Problem 5
(define matrix-transpose
	(lambda (m)
		(apply map list m)))

; Problem 6
(define last
	(lambda (ls)
		(if (null? (cdr ls))
			(car ls)
			(last (cdr ls)))))

; Problem 7
(define all-but-last
	(lambda (lst)
		(if (null? (cdr lst))
			'()
			(cons (car lst) (all-but-last (cdr lst))))))