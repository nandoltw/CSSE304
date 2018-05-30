[let-exp
	(vars (list-of symbol?))
	(vals (list-of expression?))
	(bodies (list-of expression?))
	(eval-bodies bodies
		(extend-env vars (eval-rands exps env) env))]

(define eval-bodies
	(lambda (bodies env)
		(if (null? (cdr bodies))
			(eval-exp (car bodies env))
			(begin (eval-exp (car bodies) env)
					(eval-bodies (cdr bodies) env)))))

[if-exp (test-exp then-exp else-exp)
	(if (eval-exp test-exp env)
		(eval-exp then-exp env)
		(eval-exp else-exp env))]

[lambda-exp (ids bodies)
	(closure ids bodies env)]

(define apply-proc
	(lambda (proc-value args)
		(cases proc-val proc-value
			[prim-proc 原有的东西]
			[closure (ids bodies env)
				(eval-bodies bodies
							(extend-env ids
										args
										env))])))

(define datatype closure)

