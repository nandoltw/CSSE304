; CSSE304-01 Yuankai Wang (Kevin) Assignment6
; Problem 1 
(define curry2
	(lambda (a)
		(lambda (b)
			(lambda (c)
				(a b c)))))

; Problem 2
(define curried-compose
	(lambda (proc1)
		(lambda (proc2)
			(lambda (ls)
				(proc1 (proc2 ls))))))

; Problem 3
(define compose
	(lambda list-of-functions
		(if (null? list-of-functions)
			(lambda (item)
				item)
			(lambda (item)
				(compose-list1 list-of-functions item)))))

(define compose-list1
	(lambda (list1 item)
		(if (null? list1)
			item
			((car list1) (compose-list1 (cdr list1) item)))))

; Problem 4
(define make-list-c
	(lambda (n)
		(lambda (obj)
			(letrec ([list
						(lambda (count)
							(if (zero? count)
								'()
								(cons obj (list (- count 1)))))])
				(list n)))))

; Problem 5
(define let->application
	(lambda (proc)
		(if (null? (cadr proc))
			(list (append (list 'lambda) (cdr proc)))
			(append (list (append (list 'lambda (car (apply map list (cadr proc)))) (cddr proc))) (cadr (apply map list (cadr proc)))))))

; Problem 6
(define let*->let
	(lambda (proc)
		(let-helper (cadr proc) proc)))

(define let-helper
	(lambda (list1 proc)
		(if (null? (cdr list1))
			(list 'let (list (car list1)) (caddr proc))
			(list 'let (list (car list1)) (let-helper (cdr list1) proc)))))

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