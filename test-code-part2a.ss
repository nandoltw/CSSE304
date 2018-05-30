

; -------------   test code --------------------

(define (test-occurring-vars)
    (let ([correct '(
		     (g)
		     (x)
		     (z y)
		     (g h)
		     (y x t)
		     )]
          [answers 
            (list 
	     (occurring-vars 'g)
	     (occurring-vars '(lambda (x) x))
	     (occurring-vars '((lambda (y) (lambda (x) y)) z))
	     (occurring-vars '((g h) (h g)))
	     (occurring-vars '((lambda (y) (lambda (x) y)) (x (x (t x)))))
	     )])
      (display-results correct answers  sequal?-grading)))

(define (test-lamrec-free-vars)
    (let ([correct '(
		     (x)
		     (y z)
		     (y)
		     ()
		     )]
          [answers 
            (list 
	     (free-vars '(x x))
	     (free-vars '((lambda (x) (x y)) 
			  (z (lambda (y) (z y)))))
	     (free-vars '(lambda (x) y))
	     (free-vars '((lambda (y) (lambda (y) y)) 
			  (lambda (x) (lambda (x) x))))
	     )])
      (display-results correct answers equal?)))

(define (test-lamrec-occurs-free?)
    (let ([correct '(
		     #t
		     #t
		     #t
		     )]
          [answers 
            (list 
	     (and (occurs-free? 'x 'x) 
		  (not (occurs-free? 'y 'x)))
	     (and (occurs-free? 'y '((lambda (x) (x y)) (z (lambda (y) (z y))))) 
		  (not (occurs-free? 'y '(x ((x x) x)))))
	     (and (not (occurs-free? 'x '((lambda (x) (x y)) (z (lambda (y) (z y)))))) 
		  (occurs-free? 'x '(x ((x x) x)))) 
	     )])
      (display-results correct answers equal?)))

(define (test-make-queue2)
    (let ([correct '(
		     (1 3 4 2)
		     (#t 5 3 7 #f 1 4 2)
		     )]
          [answers 
            (list 
	     (let ([q (make-queue2)] 
		   [results '()]) 
	       (q 'enqueue 2) 
	       (q 'enqueue 4)
	       (set! results (cons (q 'dequeue) results))
	       (set! results (cons (q 'dequeue) results))
	       (q 'enqueue 3)
	       (q 'enqueue 1)
	       (set! results (cons (q 'dequeue) results))
	       (q 'enqueue 5)
	       (cons (q 'dequeue) results))
	     (let ([q1 (make-queue2)] 
		   [q2 (make-queue2)] 
		   [results '()]) 
	       (q1 'enqueue 2)
	       (q1 'enqueue 4)
	       (set! results (cons (q1 'dequeue) results))
	       (set! results (cons (q1 'dequeue) results))
	       (q1 'enqueue 3)
	       (q2 'enqueue 1)
	       (set! results (cons (q2 'dequeue) results))
	       (q2 'enqueue 7)
	       (q2 'enqueue 8)
	       (q2 'enqueue 1)
	       (q1 'enqueue 5)
	       (set! results (cons (q2 'empty?) results))
	       (set! results (cons (q2 'dequeue) results))
	       (set! results (cons (q1 'dequeue) results))
	       (set! results (cons (q1 'dequeue) results))
	       (cons (q1 'empty?) results))
	     )])
      (display-results correct answers equal?)))
;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (andmap test-procedure? correct results)
          'All-correct
          `(correct: ,correct yours: ,results)))))


(define sequal?-grading
  (lambda (l1 l2)
    (cond
     ((null? l1) (null? l2))
     ((null? l2) (null? l1))
     ((or (not (set?-grading l1))
          (not (set?-grading l2)))
      #f)
     ((member (car l1) l2) (sequal?-grading
                            (cdr l1)
                            (rember-grading
                             (car l1)
                             l2)))
     (else #f))))

(define set?-grading
  (lambda (s)
    (cond [(null? s) #t]
          [(not (list? s)) #f]
          [(member (car s) (cdr s)) #f]
          [else (set?-grading (cdr s))])))

(define rember-grading
  (lambda (a ls)
    (cond
     ((null? ls) ls)
     ((equal? a (car ls)) (cdr ls))
     (else (cons (car ls) (rember-grading a (cdr ls)))))))

(define set-equals? sequal?-grading)

(define find-edges  ; e know that this node is in the graph before we do the call
  (lambda (graph node)
    (let loop ([graph graph])
      (if (eq? (caar graph) node)
	  (cadar graph)
	  (loop (cdr graph))))))

;; Problem 8  graph?
(define set?  ;; Is this list a set?  If not, it is not a graph.
  (lambda (list)
    (if (null? list) ;; it's an empty set.
	#t
	(if (member (car list) (cdr list))
	    #f
	    (set? (cdr list))))))


(define graph?
  (lambda (obj)
    (and (list? obj)
	 (let ([syms (map car obj)])
	   (and (set? syms)
		(andmap symbol? syms)
		(andmap (lambda (x)
			  (andmap (lambda (y) (member y (remove (car x) syms)))
				  (cadr x)))
			obj))))))
    
(define graph-equal?
  (lambda (a b)
    (and
     (graph? a) 
     (graph? b)
     (let ([a-nodes (map car a)]
	   [b-nodes (map car b)])
       (and 
	(set-equals? a-nodes b-nodes)
	    ; Now  See if the edges from each node are equivalent in the two graphs.
	(let loop ([a-nodes a-nodes])
	  (if (null? a-nodes)
	      #t
	      (let ([a-edges (find-edges a (car a-nodes))]
		    [b-edges (find-edges b (car a-nodes))])
		(and (set-equals? a-edges b-edges)
		     (loop (cdr a-nodes)))))))))))

(define (test-graph-equal)
  (list
   (graph-equal? '((a (b)) (b (a))) '((b (a)) (a (b))))
   (graph-equal? '((a (b c d)) (b (a c d)) (c (a b d)) (d (a b c)))
		 '((b (a c d)) (c (a b d)) (a (b d c)) (d (b a c))))
   (graph-equal? '((a ())) '((a ())))
   (graph-equal? '((a (b c)) (b (a c)) (c (a b))) '((a (b c)) (b (a c)) (c (a b))))
   (graph-equal? '() '())
   ))



(define g test-graph-equal)
	   
	  
     



;You can run the tests individually, or run them all
;#by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'occurring-vars) 
  (test-occurring-vars)
  (display 'lamrec-free-vars) 
  (test-lamrec-free-vars)
  (display 'lamrec-occurs-free?) 
  (test-lamrec-occurs-free?)
  (display 'make-queue2) 
  (test-make-queue2)

)

(define r run-all)


