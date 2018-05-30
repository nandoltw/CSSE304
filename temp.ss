(define aaa
	(lambda (lst stack)
		(if (null? lst)
			stack
			(aaa (cdr lst) (cons (list (car lst)) stack)))))