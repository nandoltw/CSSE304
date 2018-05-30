(define apply-continuation
	(lambda (k v)
		(k v)))

(define fact-acc
	(lambda (n acc)
		(if (zero? n)
			acc
			(fact-acc (- n 1) (* acc n)))))

(fact-acc 5 1)

(define fact-cps
	(lambda (n k)
		(if (zero? n)
			(apply-continuation k 1)
			(fact-cps (- n 1)
				(lambda (v) (apply-continuation k (* n v)))))))

(fact-cps 5 list)

(define memq-cps
	(lambda (sym ls k)
		(cond [(null? ls) (apply-continuation k #f)]
			  [(eq? (car ls) sym)
			  	(apply-continuation k #t)]
			  [else (memq-cps sym (cdr ls) k)])))