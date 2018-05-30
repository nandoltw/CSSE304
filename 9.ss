; CSSE304-01 Yuankai Wang (Kevin) Assignment9
; Problem 1 Get help from Jizhou Huang
(define snlist-recur
	(lambda (base-case car-pred cdr-pred)
		(letrec ([helper 
					(lambda (slist)
						(cond [(null? slist) base-case]
		 					  [(list? slist) (apply car-pred (map (lambda (x)
																		(if (list? x)
																			(helper x)
																			(cdr-pred x))) slist))]
		 					  [else (car-pred (cdr-pred slist))]))])
		helper)))

;a
(define sn-list-sum
	(lambda (snlst)
		((snlist-recur 0 + (lambda (x) x)) snlst)))

;b
(define sn-list-map
	(lambda (proc snlst)
		((snlist-recur '() list proc) snlst)))

;c
(define sn-list-paren-count
	(lambda (snlst)
		((snlist-recur 2
					   (lambda x (+ 2 (apply + x)))
					   (lambda (x) 0)) snlst)))

;d
(define sn-list-reverse 
	(lambda (snlst)
		((snlist-recur '() (lambda x (reverse x)) (lambda (x) x)) snlst)))

;e
(define sn-list-occur
	(lambda (s snlst)
		((snlist-recur 0 (lambda x (apply + x)) (lambda (x) (if (equal? x s)
															    1
															    0))) snlst)))

;f
(define sn-list-depth
	(lambda (snlst)
		((snlist-recur 1 (lambda x (+ 1 (apply max x))) (lambda (x) 0)) snlst)))

; Problem 2
(define bt-recur
	(lambda (base-case car-pred cdr-pred)
		(letrec ([helper 
					(lambda (bt)
						(cond [(null? bt) base-case]
		 					  [(list? bt) (car-pred (car bt) (helper (cadr bt)) (helper (caddr bt)))]
		 					  [else (cdr-pred bt)]))])
		helper)))

(define bt-sum
	(lambda (T)
		((bt-recur 0 (lambda (this left right) 
						(+ left right))
					 (lambda (x)
					 	(if (integer? x)
					 		x
					 		0))) T)))

(define bt-inorder
	(lambda (T)
		((bt-recur '() (lambda (this left right) 
						(append left (cons this right)))
					   (lambda (x) '())) T)))

; Problem 3
(define make-c...r
	(lambda (str)
		(let ([str-list (string->list str)])
			(apply compose (map (lambda (x) (if (equal? x #\a)
												car
										 		cdr)) str-list)))))

(define compose
	(case-lambda
		[() (lambda (x) x)]
		[(first . rest)
			(let ([composed-rest (apply compose rest)])
				(lambda (x) (first (composed-rest x))))]))