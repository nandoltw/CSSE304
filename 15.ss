; Problem 2/(2): The code I write is a top-down version. I main save the calculation times, but I still need to call every value when I
; get there. The procedure calls take time. There are many duplicates during the call. So it is not that fast.
; The code wrote in class is bottom-up version. It avoid duplicate calls to the same value. It build all the way through. So it will
; never get back to smaller values. 

; CSSE304-01 Yuankai Wang (Kevin) Assignment15
; Problem 1 
(define apply-continuation
	(lambda (k v)
		(k v)))
; a
(define member?-cps
	(lambda (item list continuation)
		(if (null? list)
			(apply-continuation continuation #f)
			(if (equal? item (car list))
				(apply-continuation continuation #t)
				(member?-cps item (cdr list) continuation)))))

; b
(define set?-cps
	(lambda (ls continuation)
		(cond [(null? ls) (apply-continuation continuation #t)]
		      [(not (pair? ls)) (apply-continuation continuation #f)]
		      [else (member?-cps (car ls) (cdr ls) 
		      			(lambda (x)
		      				(if x
		      					(apply-continuation continuation #f)
		      					(set?-cps (cdr ls) continuation))))])))

; c
(define set-of-cps
	(lambda (s continuation)
		(cond [(null? s) (apply-continuation continuation '())]
			  [else (member?-cps (car s) (cdr s)
			  			(lambda (x)
			  				(set-of-cps (cdr s) 
			  					(lambda (y)
			  						(if x
			  							(apply-continuation continuation y)
			  							(apply-continuation continuation (cons (car s) y)))))))])))

(define map-cps
	(lambda (proc-cps L continuation)
		(cond [(null? L) (apply-continuation continuation '())]	
			  [else (map-cps proc-cps (cdr L)
			  			(lambda (x)
			  				(proc-cps (car L)
			  					(lambda (y)
			  						(apply-continuation continuation (cons y x))))))])))  

(define domain-cps
  	(lambda (rel continuation)
  		(map-cps 1st-cps rel 
  			(lambda (x)
				(set-of-cps x continuation)))))

(define 1st-cps
	(lambda (L continuation)
		(apply-continuation continuation (car L))))

; d
(define make-cps
	(lambda (proc)
		(lambda (L continuation)
			(apply-continuation continuation (proc L)))))

; e
(define andmap-cps
	(lambda (pred-cps list continuation)
		(cond [(null? list) (apply-continuation continuation #t)]
			  [else (1st-cps list
			  			(lambda (x)
			  				(pred-cps x
			  					(lambda (y)
									(if y
										(andmap-cps pred-cps (cdr list) continuation)
										(apply-continuation continuation #f))))))])))

; f
(define snlist-recur
	(lambda (seed item-proc list-proc)
		(letrec ([helper
			(lambda (ls)
				(if (null? ls)
					seed
					(let ([c (car ls)])
						(if (or (pair? c) (null? c))
							(list-proc (helper c) (helper (cdr ls)))
							(item-proc c (helper (cdr ls)))))))])
		helper)))

(define cps-snlist-recur
	(lambda (base-value item-proc-cps list-proc-cps)
		(letrec ([helper
			(lambda (ls continuation)
				(cond [(null? ls)
							(apply-continuation continuation base-value)]
					  [(or (pair? (car ls)) (null? (car ls)))
					  		(helper (car ls)
					  		  	(lambda (x)
					  				(helper (cdr ls)
					  					(lambda (y)
					  						(list-proc-cps x y continuation)))))]
					  [else (1st-cps ls
					  			(lambda (x)
					  				(helper (cdr ls)
					  					(lambda (y)
					  						(item-proc-cps x y continuation)))))]))])
		helper)))

(define +-cps
	(lambda (a b continuation)
		(apply-continuation continuation (+ a b))))

(define sn-list-depth-cps
	(cps-snlist-recur
		1
		(lambda (x y continuation)
		    (apply-continuation continuation y))
		(lambda (x y continuation)
		    (apply-continuation continuation (max (+ x 1) y)))))

(define sn-list-reverse-cps
	(cps-snlist-recur
		'()
		(lambda (x y continuation)
		    (apply-continuation continuation (append y (list x))))
		(lambda (x y continuation)
		    (apply-continuation continuation (append y (list x))))))

(define sn-list-occur-cps
	(lambda (sym slist continuation)
    	((cps-snlist-recur 
    		0
		    (lambda (x y continuation)
				(apply-continuation continuation 
			 					 	(if (eq? x sym)
						    			(+ y 1)
						    		 	y)))
		    +-cps)
    	slist continuation)))

; Problem 2
(define memoize
	(lambda (f hash equiv?)
		(let* ([htable (make-hashtable hash equiv?)]
			  [contains? (lambda (key)
				(hashtable-contains? htable key))]
	   		  [set (lambda (key value)
		  		(hashtable-set! htable key value))]
	   		  [get (lambda (key)
		  		(hashtable-ref htable key '()))])
		(lambda input
			(if (contains? input)
				(get input)
				(let ([temp-result (apply f input)])
					(set input temp-result)
					temp-result))))))

; Problem 3
(define subst-leftmost
	(lambda (new old slist equality-pred?)
		(call-with-values 
			(lambda () 
				(subst-leftmost-changed new old slist equality-pred? 0))
			(lambda (output found) output))))

(define subst-leftmost-changed
	(lambda (new old slist equality-pred? changed)
		(cond [(not (zero? changed)) (values slist 1)]
			  [(null? slist) (values '() 0)]
			  [(list? (car slist))
			  	(call-with-values
			  		(lambda () 
			  			(subst-leftmost-changed new old (car slist) equality-pred? changed))
			  		(lambda (output found)
			  			(if (zero? found)
			  				(call-with-values
			  					(lambda ()
			  						(subst-leftmost-changed new old (cdr slist) equality-pred? found))
			  					(lambda (output found)
			  						(values (cons (car slist) output) found)))
			  				(values (cons output (cdr slist)) 1))))]
			  [else (if (equality-pred? (car slist) old)
			  			(call-with-values
			  				(lambda ()
			  					(subst-leftmost-changed new old (cdr slist) equality-pred? 1))
			  				(lambda (output found)
			  					(values (cons new output) 1)))
			  			(call-with-values
			  				(lambda ()
			  					(subst-leftmost-changed new old (cdr slist) equality-pred? 0))
			  				(lambda (output found)
			  					(values (cons (car slist) output) found))))])))