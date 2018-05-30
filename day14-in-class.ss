(define-syntax my-if
	(syntax-rules (then else)
		[(_ e1 then e2) (if e1 e2)]
		[(_ e1 then e2 else e3) (if e1 e2 e3)]))

(define-syntax ++
	(syntax-rules ()
		[(_ x) (begin (set! x (add1 x)) x)]))

(define-syntax ++post
	(syntax-rules ()
		[(_ x) (let ([new x])
				(++x)
				new)]))

(define-syntax my-and
	(syntax-rules ()
		[(_) #t]
		[(_ e) e]
		[(_ e1 e2 ...) (if e1
						   (my-and e2 ...))]))

(define-syntax for
	(syntax-rules (:)
		[(_ ((init ...) : test : update ...) body ...)
		 (begin
		 	init ...
		 	(let for-loop ()
		 		(begin
		 			body ...
		 			update ...
		 			(for-loop))))]))

(for (((define i 0) (define j 1)) : (< i 6) : (++ i) (set! j (* 2 j))) (display i) (display " ") (display j) (newline))