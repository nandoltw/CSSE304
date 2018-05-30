; CSSE304-01 Yuankai Wang (Kevin) Assignment8
; Problem 1a
(define slist-map
	(lambda (proc slist)
		(if (null? slist)
			'()
			(if (null? (car slist))
				(cons '() (slist-map proc (cdr slist)))
				(if (list? (car slist))
					(if (null? (cdr slist))
						(list (slist-map proc (car slist)))
						(cons (slist-map proc (car slist)) (slist-map proc (cdr slist))))
					(cons (proc (car slist)) (slist-map proc (cdr slist))))))))

; Problem 1b
(define slist-reverse 
	(lambda (slist)
		(if (null? slist)
			'()
			(if (null? (car slist))
				(append (slist-reverse (cdr slist)) (list '()))
				(if (list? (car slist))
					(append (slist-reverse (cdr slist)) (list (slist-reverse (car slist))))
					(append (slist-reverse (cdr slist)) (list (car slist))))))))

; Problem 1c
(define slist-paren-count
	(lambda (slist)
		(if (null? slist)
			2
			(if (list? (car slist))
				(+ (slist-paren-count (cdr slist)) (slist-paren-count (car slist)))
				(slist-paren-count (cdr slist))))))

; Problem 1d
(define slist-depth
	(lambda (slist)
		(if (null? slist)
			1
			(if (list? (car slist))
				(if (list? (cdr slist))
					(max (+ 1 (slist-depth (car slist))) (slist-depth (cdr slist)))
					(+ 1 (slist-depth (car slist))))
				(slist-depth (cdr slist))))))

; Problem 1e
(define slist-symbols-at-depth
	(lambda (slist d)
		(if (null? slist)
			'()
			(if (equal? d 1)
				(if (list? (car slist))
					(slist-symbols-at-depth (cdr slist) d)
					(cons (car slist) (slist-symbols-at-depth (cdr slist) d)))
				(if (list? (car slist))
					(append (slist-symbols-at-depth (car slist) (- d 1)) (slist-symbols-at-depth (cdr slist) d))
					(slist-symbols-at-depth (cdr slist) d))))))

; Problem 2
(define group-by-two
	(lambda (ls)
		(if (null? ls)
			'()
			(if (< (length ls) 2)
				(list ls)
				(append (list (list (car ls) (cadr ls))) (group-by-two (cddr ls)))))))

; Problem 3
(define make-stack
	(lambda ()
		(let ([stk '()])
			(lambda (msg  . args )
				(case msg [(empty?) (null? stk)]
					[(push) (set! stk (cons (car args) stk))]
					[(pop) (let ([top (car stk)])
								(set! stk (cdr stk))
								top)]
					[else (errorf 'stack "illegal message to stack object: ~a" msg)])))))

(define make-slist-leaf-iterator
	(lambda (slist)
		(let ([s (make-stack)])
			(s 'push slist)
			(letrec ([get-next
				(lambda (msg)
					(case msg
						[(next) (if (s 'empty?)
									#f
									(let ([temp (s 'pop)])
										(if (not (null? (cdr temp)))
											(s 'push (cdr temp)))
										(if (list? (car temp))
											(if (null? (car temp))
												(get-next 'next)
												(begin (s 'push (car temp))
													 (get-next 'next)))
											(car temp))))]
						[else (errorf 'iter "illegal message to slist-leaf-iterator: ~a" msg)]))])
				get-next))))

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