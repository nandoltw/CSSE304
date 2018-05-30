; -------------    Starting code --------------------

; C2: lamrec

(define lamrec
  (lambda (vf lf af)
    (letrec ((helper 
	      (lambda (exp)
		(cond
		 ((symbol? exp) (vf exp))
		 ((eq? (car exp) 'lambda)
		  (lf (caadr exp) (helper (caddr exp))))
		 (else (af (helper (car exp)) 
			   (helper (cadr exp))))))))
      helper)))

;C3 make-queue.  Don't forget to call your new procedure make-queue2.

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

; C4 trace-lambda starting code

(define display-traced-output
  (let ([multi-indent-string
	 (lambda (level)
	   (let loop ([level level] [result ""])
	     (if (zero? level)
		 result
		 (loop (- level 1) (string-append result "| ")))))])
    (lambda args; (level proc-name args) or (level answer)
      (let ([indent-string (multi-indent-string (car args))])
	(display indent-string)
	(display (if (= 2(length args)) ; otherwise the length is 3
		     (cadr args)   ; display the return value of a call
		     (cons (cadr args) (caddr args))))) ; display the args
      (newline))))

; C4-trace-lambda-tests - uncomment before using.  
; Use them with your interpreter in rep mode

'(let ([a (trace-lambda aa (n)
              (+ 1 n))])
     (let ([b (trace-lambda bb (n m)
                 (+ m (a n)))])
       ((trace-lambda cc (n)
          (+ n (b n (a n))))
        (a 3))))


'(let ([a 2])
    (let ([g (let ([b (+ a 3)])
               (trace-lambda g (x y) 
                  (+ x y b)))])
       ((trace-lambda r (x) (g (g x 2) 1))
        7)))))

'(let ([f1 (trace-lambda f1 (x y) (+ x y))]
         [f2 (lambda (n) (+ n 3))]
         [f3 (trace-lambda f3 (t) (+ 8 t))])
    (f1 (f1 (f2 (f3 4)) (f2 5)) 6))

'(let ([f1 (trace-lambda f1 (b c) (+ b c))]
         [f2 (trace-lambda f2 (x) (* x x ))])
     (let ([f3 (trace-lambda f3 (x y) 
                 (f2 (f1 (+ x y) 1)))])
       (let ([f4 (trace-lambda f4 (x) 
                   (f3 (f2 4) (f3 x 3)))])
          (f4 (f4 2)))))




