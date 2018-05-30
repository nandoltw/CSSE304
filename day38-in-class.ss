; (define (one? n) (zero? (sub1 n)))
; (define (sub2 n) (sub1 (sub1 n)))

; (define-datatype kt kt?
; 	(empty-k)
; 	(fib-inner-k (v number?)
; 				 (k kt?))
; 	(fib-outer-k (n number?)
; 				 (k kt?)))

; (define (apply-k k val)
; 	(cond [(procedure? k) (k val)]
; 		  [else (cases kt k
; 		  			[empty-k () val]
; 		  			[fib-inner-k (v k2) (apply-k k2 (+ v val))]
; 		  			[fib-outer-k (n k3) (fib-cps (sub2 n) (fib-inner-k val k3))])]))

; (define fib-cps
; 	(lambda (n k)
; 		(cond [(or (zero? n) (one? n)) (apply-k k n)]
; 			  [else
; 			  	(fib-cps (sub1 n)
; 			  				(fib-outer-k n k))])))

; (pretty-print (fib-cps 7 (empty-k)))

;llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll
; (define (one? n) (zero? (sub1 n)))
; (define (sub2 n) (sub1 (sub1 n)))

; (define-datatype kt kt?
; 	(empty-k (j procedure?))
; 	(fib-inner-k (v number?)
; 				 (k kt?))
; 	(fib-outer-k (n number?)
; 				 (k kt?)))

; (define (apply-k k val)
; 	(cond [(procedure? k) (k val)]
; 		  [else (cases kt k
; 		  			[empty-k (j) (j val)]
; 		  			[fib-inner-k (v k2) (lambda () (apply-k k2 (+ v val)))]
; 		  			[fib-outer-k (n k3) (lambda () (fib-cps (sub2 n) (fib-inner-k val k3)))])]))

; (define fib-cps
; 	(lambda (n k)
; 		(cond [(or (zero? n) (one? n)) (lambda () (apply-k k n))]
; 			  [else
; 			  	(lambda () (fib-cps (sub1 n) (fib-outer-k n k))])))

; (define (trampoline th)
; 	(trampoline (th)))

; (define (bi-trampoline th th1)
; 	(bi-trampoline th1 (th)))

; ; Only need one infinite loop to do all the computation
; (call/cc (lambda (j) (bi-trampoline (lambda () (fib-cps -1 (empty-k j)))
; 									(lambda () (fib-cps 7 (empty-k j))))))

;llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll

(define (one? n) (zero? (sub1 n)))
(define (sub2 n) (sub1 (sub1 n)))

(define-datatype kt kt?
	(empty-k (j procedure?))
	(fib-inner-k (v number?)
				 (k kt?))
	(fib-outer-k (n number?)
				 (k kt?)))

(define (apply-k k val)
	(cond [(procedure? k) (k val)]
		  [else (cases kt k
		  			[empty-k (j) (j val)]
		  			[fib-inner-k (v k2) (lambda () (apply-k k2 (+ v val)))]
		  			[fib-outer-k (n k3) (lambda () (fib-cps (sub2 n) (fib-inner-k val k3)))])]))

(define fib-cps
	(lambda (n k)
		(cond [(or (zero? n) (one? n)) (lambda () (apply-k k n))]
			  [else
			  	(lambda () (fib-cps (sub1 n) (fib-outer-k n k))])))

(begin 
	(set! k (fib-outer-k n k))
	(set! v (sub1 n))
	(set! pc fib-cps))

(define (trampoline th)
	(trampoline (th)))

(define (bi-trampoline th th1)
	(bi-trampoline th1 (th)))

; Only need one infinite loop to do all the computation
(call/cc (lambda (j) (bi-trampoline (lambda () (fib-cps -1 (empty-k j)))
									(lambda () (fib-cps 7 (empty-k j))))))