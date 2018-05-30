(load "chez-init.ss")

(define-datatype kontinuation kontinuation?
	[init-k]
	[flatten-cdr-k (ls list?)
				   (k kontinuation?)]
	[flatten-car-k  (flattened-cdr list?)
					(k kontinuation?)]
  	[append-k (car-L1 symbol?)
  			  (k kontinuation?)])

(define apply-k
  	(lambda ()
	 	(cases kontinuation k
	    	[init-k ()
	       		(pretty-print v)]
	    	[flatten-cdr-k (l k1)
	       		(if (list? (car l))
	       			(begin (set! k (flatten-car-k v k1))
	       				   (set! ls (car l))
	       				   (flatten-cps))
	       			(begin (set! v (cons (car l) v))
	       				   (set! k k1)
	       				   (apply-k)))]
	    	[flatten-car-k (flattened-cdr k2)
	    		(begin (set! L1 v)
	    			   (set! L2 flattened-cdr)
	    			   (set! k k2)
	    			   (append-cps))]
	    	[append-k (car-L1 k3)
	    		(begin (set! v (cons car-L1 v))
	    			   (set! k k3)
	    			   (apply-k))])))

(define append-cps 
	(lambda ()
    	(if (null? L1)
    		(begin (set! v L2)
    			   (apply-k))
    		(begin (set! k (append-k (car L1) k))
    			   (set! L1 (cdr L1))
    			   (append-cps)))))

'(trace append-cps flatten-cps apply-k)

(define flatten-cps
  	(lambda ()
    	(if (null? ls)
    		(begin (set! v ls)
    			   (apply-k)
    			   v)
			(begin (set! k (flatten-cdr-k ls k))
				   (set! ls (cdr ls))
				   (flatten-cps)))))