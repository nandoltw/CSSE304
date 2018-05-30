; CSSE304-01 Yuankai Wang (Kevin) Assignment6a
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