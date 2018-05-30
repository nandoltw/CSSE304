; CSSE304-01 Yuankai Wang (Kevin) Assignment8b
; Problem 4
(define group-by-n
	(lambda (ls n)
		(group-by-n-counter ls n 0 '())))

(define group-by-n-counter
	(lambda (ls n counter temp)
		(if (null? ls)
			(if (null? temp)
				'()
				(list (reverse temp)))			
			(if (< counter n)
				(group-by-n-counter (cdr ls) n (+ counter 1) (cons (car ls) temp))
				(cons (reverse temp) (group-by-n-counter ls n 0 '()))))))

; Problem 5
(define subst-leftmost
	(lambda (new old slist equality-pred?)
		(car (subst-leftmost-changed new old slist equality-pred? 0))))

(define subst-leftmost-changed
	(lambda (new old slist equality-pred? changed)
		(cond [(not (zero? changed)) (list slist 1)]
			  [(null? slist) (list '() 0)]
			  [(list? (car slist))
			  		(let ([new-changed (subst-leftmost-changed new old (car slist) equality-pred? changed)])
			  			(if (zero? (cadr new-changed))
			  				(let ([next-part (subst-leftmost-changed new old (cdr slist) equality-pred? (cadr new-changed))])
				  				(list (cons (car new-changed) (car next-part)) (cadr next-part)))
						  	(list (cons (car new-changed) (cdr slist)) 1)))]
			  [else (if (equality-pred? (car slist) old)
			  			(let ([temp (subst-leftmost-changed new old (cdr slist) equality-pred? 1)])
							(list (cons new (car temp)) 1))
			  			(let ([temp (subst-leftmost-changed new old (cdr slist) equality-pred? 0)])
							(list (cons (car slist) (car temp)) (cadr temp))))])))