; CSSE304-01 Yuankai Wang (Kevin) Assignment6b
; Problem 7
(define filter-in
	(lambda (pred? lst)
		(filter-in-output pred? lst '())))

(define filter-in-output
	(lambda (pred ls output)
		(if (null? ls)
			output
			(if (pred (car ls))
				(filter-in-output pred (cdr ls) (append output (list (car ls))))
				(filter-in-output pred (cdr ls) output)))))

; Problem 8
(define filter-out
	(lambda (pred? lst)
		(filter-out-output pred? lst '())))

(define filter-out-output
	(lambda (pred ls output)
		(if (null? ls)
			output
			(if (pred (car ls))
				(filter-out-output pred (cdr ls) output)
				(filter-out-output pred (cdr ls) (append output (list (car ls))))))))

; Problem 9
(define sort-list-of-symbols
	(lambda (los)
		(map string->symbol (sort string<? (map symbol->string los)))))

; Problem 10
(define invert
	(lambda (lst)
		(map reverse lst)))

; Problem 11
(define vector-index
	(lambda (pred ls)
		(vector-index-count pred (vector->list ls) 0)))

(define vector-index-count
	(lambda (pred ls count)
		(if (null? ls)
			#f
			(if (pred (car ls))
				count
				(vector-index-count pred (cdr ls) (+ count 1))))))

(define vector-index
	(lambda (pred ls)
		(letrec ([vector-index-count
					(lambda (pred ls count)
						(if (null? ls)
							#f
							(if (pred (car ls))
								count
								(vector-index-count pred (cdr ls) (+ count 1)))))])
			(vector-index-count pred (vector->list ls) 0))))

(define vector-index
	(lambda (pred ls)
		(let vector-index-count ([ls (vector->list ls)] [count 0])
			(if (null? ls)
				#f
				(if (pred (car ls))
					count
					(vector-index-count (cdr ls) (+ count 1)))))))


; Problem 12
(define ribassoc 
	(lambda (s los v fail-value)
		(ribassoc-list s los (vector->list v) fail-value)))

(define ribassoc-list
	(lambda (s los v fail-value)
		(if (null? los)
			fail-value
			(if (equal? s (car los))
				(car v)
				(ribassoc-list s (cdr los) (cdr v) fail-value)))))