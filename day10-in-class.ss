(define list-recur
	(lambda (base-value list-proc)
		(letrec ([helper
					(lambda (ls)
						(if (null? ls)
							base-value
							(list-proc (car ls)
								(helper (cdr ls)))))])
		helper)))

(define filter
	(lambda (proc)
		(list-recur '()
					(lambda (x y)
						(if (proc x)
							(cons x y)
							y)))))