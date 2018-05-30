(load "chez-init.ss")

(define-datatype kontinuation kontinuation?
  [init-k]
  [append-k (L1 list?)
	    (k (lambda (x) (or (kontinuation? x)(procedure? x))))]
  [flatten-cdr-k (L list?)
		 (k (lambda (x) (or (kontinuation? x)(procedure? x))))]
  [flatten-car-k (flattened-cdr list?)
		 (k kontinuation?)]
)

(define apply-k 
 (lambda (k v)
  (if (procedure? k)
      (k v)
      (cases kontinuation k
	[init-k ()
	    (pretty-print v)
	    (read-flatten-print)]
	[append-k (L1 k)
	    (apply-k k (cons (car L1) v))]
	[flatten-cdr-k (L k)
	    (if (list? (car L))
		(flatten-cps (car L) (flatten-car-k v k))		
		(apply-k k (cons (car L) v)))]
	[flatten-car-k (flattened-cdr k)
		(append-cps v flattened-cdr k)]       
	))))

(define flatten-cps
  (lambda (ls k)
    (if (null? ls)
	(apply-k k ls)
	(flatten-cps (cdr ls) (flatten-cdr-k ls k)))))

(define append-cps 
  (lambda (L1 L2 k)
    (if (null? L1)
	(apply-k k L2)
	(append-cps (cdr L1)
		    L2
		    (append-k L1 k)))))

(define read-flatten-print
  (lambda ()
    (display "enter slist to flatten: ")
    (let ([slist (read)])
      (unless (eq? slist 'exit)
	(flatten-cps slist (init-k))))))

;(trace append-cps flatten-cps apply-k)



;;;starting code
; (load "chez-init.ss")

; (define apply-k (lambda (k . v) (apply k v)))

; (define read-flatten-print
;   (lambda ()
;     (display "enter slist to flatten: ")
;     (let ([slist (read)])
;       (unless (eq? slist 'exit)
; 	(flatten-cps slist 
; 		     (lambda (val)
; 		       (pretty-print val)
; 		       (read-flatten-print)))))))

; (define flatten-cps
;   (lambda (ls k)
;     (if (null? ls)
; 	(apply-k k ls)
; 	(flatten-cps (cdr ls)
; 	  (lambda (v) (if (list? (car ls))
; 			  (flatten-cps (car ls)
; 			      (lambda (u) (append-cps u v k)))
; 			  (apply-k k (cons (car ls) v))))))))

; (define append-cps 
;   (lambda (L1 L2 k)
;     (if (null? L1)
; 	(apply-k k L2)
; 	(append-cps (cdr L1)
; 		    L2
; 		    (lambda (appended-cdr)
; 		      (apply-k k (cons (car L1)
; 				       appended-cdr)))))))

; ;(trace append-cps flatten-cps apply-k)