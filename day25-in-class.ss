(define intersection-CPS
	(lambda (los1 los2 k)
		(cond [(null? los1) (apply-continuation k '())]
			  [else (intersection-CPS (cdr los1) los2
			  			(lambda (cdr-intersection)
			  				(memq-CPS (car los1) los2
			  					(lambda (car-in-los2)
			  						(apply-continuation k (if car-in-los2
			  												  (cons (car los1) cdr-intersection) cdr-intersection))))))])))

(define free-vars-CPS
	(lambda (exp k)
		(cond [(symbol? exp) (apply-continuation k (list exp))]
			  [(eq? (1st exp) 'lambda) (free-vars-CPS (3rd exp)
			  								(lambda (body-free-vars)
			  									(remove-CPS (car (2nd exp)) k)))]
			  [else (free-vars-CPS (1st exp) (lambda (rator-from-vars)
			  									(free-vars-CPS (2nd exp)
			  										(lambda (rand-from-vars)
			  											(union-CPS rator-from-vars
			  													   rand-from-vars
			  													   k)))))])))

succeed:
[else (prod-CPS
		(cdr L)
		(lambda (cdr-prod)
				(apply-continuation
					succeed
					(* (car ls) cdr prod))
				fail))]