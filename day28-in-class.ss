(define deref
	cell-ref)

(define set-ref!
	cell-set!)

(define apply-env-ref
	(lambda (env val)
; same code we had before apply-env
		))

(define apply-env
	(lambda (env var) 
		(deref (apply-env-ref env var))))



(define cell
	(lambda (x)
		(cons x 'this-is-a-cell)))

(define cell-ref
	car)

(define cell-set!
	set-car!)

(define cell?
	(lambda (obj)
		(and (pair? obj) (eq? (cdr obj) 'this-is-a-cell))))