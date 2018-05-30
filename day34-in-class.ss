(define eval-exp
	(lambda (exp env k)
		(cases expression exp
			.
			.
			.
			[let-exp (vars exps bodies)
				(eval-rands exps
							env
							(let-exps-k vars bodies env k))])))

(define-datatype continuation continuation?
	[let-exps-k (vars (list-of symbol?))
				(bodies (list-of expression?))
				(env environmemt?)
				(k continuation?)]
	[let-extend-k (bodies (list-of expression?))
				  ;(env environmemt?)
				  (k continuation?)])

(define apply-k
	(lambda (k v)
		(cases continuation k
			[test-k ...]
			...
			[let-exps-k (vars bodies env k)
				(extend-env vars
							v
							env
							(let-extend-k bodies k))]
			[let-extend-k (bodies k)
				(eval-bodies bodies v k)]
			)))

;需要cps的eval-exp, call eval-exp
;应该不用 extend-env parse syntax-expand environment

;call/cc

(define datatype proc-val proc-val?
	[continuation-proc (k continuation?)])

(define apply-prim-proc
	(lambda (p-p args k)
		(case p-p
			[(car)...]
			.
			.
			.
			[(call/cc) (apply-proc (1st args) (list (continuation-proc k)) k)])))

(define apply-proc
	(lambda (proc-val args k)
		(cases ...


	[continuation-proc (k);把一开始那个k ignore了
		(apply-k k (car args))])))