; CSSE304-01 Yuankai Wang (Kevin) Exam2
; Problem 1
;Get from HW10
(define no-duplicate
  (lambda (lst output)
    (if (null? lst)
      output
      (if (member (car lst) output)
        (no-duplicate (cdr lst) output)
        (no-duplicate (cdr lst) (cons (car lst) output))))))

(define bound-vars
  (lambda (e)
    (no-duplicate (bound-vars-output e '()) '())))

(define bound-vars-output
  (lambda (e output)
    (cond [(null? e) output]
          [(symbol? e) output]
          [(equal? (car e) 'lambda) (if (list? (caddr e))
                                        (let ([next (bound-vars-output (caddr e) '())])
                                          (if (null? next)
                                            (let ([next2 (free-vars-output (caddr e) '())])
                                              (if (and (not (null? next2)) (equal? (car next2) (caadr e)))
                                                (cons (car next2) output)
                                                output))
                                            (append (bound-vars (list (car e) (cadr e) (car next))) output)))
                                        (if (and (equal? (caddr e) (caadr e)) (not (null? (free-vars (caddr e)))))
                                           (cons (caddr e) output)
                                           output))]
          [else (append (bound-vars-output (car e) '()) (bound-vars-output (cdr e) '()) output)])))

(define occurring-vars-dup
	(lambda (expr)
		(cond
			[(symbol? expr) (free-vars expr)]
			[(and (list? expr) (equal? 'lambda (car expr))) (occurring-vars (caddr expr))]
			[else (append (occurring-vars (car expr)) (occurring-vars (cadr expr)))])))

(define occurring-vars
	(lambda (expr)
		(no-duplicate (occurring-vars-dup expr) '())))

; Problem 2
(define lamrec
	(lambda (vf lf af)
	    (letrec ((helper 
		    	(lambda (exp)
					(cond
						[(symbol? exp) (vf exp)]
			 			[(eq? (car exp) 'lambda)
			  				(lf (caadr exp) (helper (caddr exp)))]
			 			[else (af (helper (car exp)) 
				   			(helper (cadr exp)))]))))
	    helper)))

(define free-vars
	(lambda (e)
		(no-duplicate (free-vars-output e '()) '())))

(define free-vars-output
	(lambda (e output)
		((lamrec (lambda (n) (cons n output))
				(lambda (var exp) (filter (lambda (n) (not (equal? var n))) exp))
				append) e)))

(define occurs-free?
	(lambda (var exp)
		((lamrec (lambda (n) (equal? var n))
				 (lambda (a exp) (and (not (equal? var a)) exp))
				 (lambda (x y) (or x y))) exp)))

; Problem 3
(define make-queue
  (lambda ()
    (let ([q-f '()] [q-r '()])
      (lambda (msg . args)
	(case msg
	  [(empty?) (if (null? q-f) (begin (set! q-f (reverse q-r))
					   (set! q-r '())))
	   (null? q-f)]
	  [(enqueue) (set! q-r (cons (car args) q-r))]
	  [(dequeue) (if (null? q-f) (begin (set! q-f (reverse q-r))
					    (set! q-r '())))
	   (cond
	    [(null? q-f) (errorf 'queue "attempt to dequeue from empty queue")]
	    [else (let ((h (car q-f)))
		    (set! q-f (cdr q-f))
		    h)])]
	  [(peek)
	   (if (null? q-f) (begin (set! q-f (reverse q-r))
				  (set! q-r '())))
	   (cond
	    [(null? q-f) (errorf 'queue "attempt to peek from empty queue")]
	    [else (car q-f)])]
	  [else (errorf 'queue "illegal message to queue object: ~a" msg)])))))

(define-syntax rever
	(syntax-rules () [(_ null-ele other-ele)
		(if (null? null-ele) (begin (set! null-ele (reverse other-ele))
					   				 (set! other-ele '())))]))

; (define rev
; 	(lambda (null-ele other-ele)
; 		(if (null? null-ele) (begin (set! null-ele (reverse other-ele))
; 					   				 (set! other-ele '())))))

(define make-queue2
	(lambda ()
    (let ([q-f '()] [q-r '()])
      (lambda (msg . args)
	(case msg
	  [(empty?) (rever q-f q-r)
	   (null? q-f)]
	  [(enqueue) (set! q-r (cons (car args) q-r))]
	  [(dequeue) (rever q-f q-r)
	   (cond
	    [(null? q-f) (errorf 'queue "attempt to dequeue from empty queue")]
	    [else (let ((h (car q-f)))
		    (set! q-f (cdr q-f))
		    h)])]
	  [(peek)
	   (rever q-f q-r)
	   (cond
	    [(null? q-f) (errorf 'queue "attempt to peek from empty queue")]
	    [else (car q-f)])]
	  [else (errorf 'queue "illegal message to queue object: ~a" msg)])))))

