;; Test code 


(define (test-extend)
    (let ([correct '(
		     #t
		     #t
		     (0 1 2 4)
		     (0 1 2 3 4)
		     (0 1 2 3 4)
		     )]
          [answers 
            (list 
	     (and ((extend 1 even?) 0) ((extend 1 even?) 1))
	     (and ((extend 3 even?) 2) (not ((extend 3 even?) 1)))
	     (filter (extend 1 even?) '(0 1 2 3 4 5))
	     (filter (extend 3 (extend 1 even?)) '(0 1 2 3 4 5))
	     (filter (extend 7 (extend 3 (extend 1 even?))) '(0 1 2 3 4 5))
	     )])
      (display-results correct answers equal?)))

(define (test-takef)
    (let ([correct '(
		     (0)
		     ((0) (1) (2) (3))
		     ()
)]
          [answers 
            (list 
	     (takef even? '(0 1 2 3 4))
	     (takef pair? '((0) (1) (2) (3) 4 5 6))
	     (takef positive? '(-2 3 4 5))
)])
      (display-results correct answers equal?)))


(define (test-add-between)
    (let ([correct '(
		     (1 and 2 and 1 and 2)
		     ()
		     (1 and 10)
		     (1)
		     )]
          [answers 
            (list 
	     (add-between 'and '(1 2 1 2))
	     (add-between 'and '())
	     (add-between 'and '(1 10))
	     (add-between 'or '(1))
)])
      (display-results correct answers equal?)))

(define (test-binary->natural)
    (let ([correct '(
		     0
		     4
		     12
		     15
		     21
		     8191
		     )]
          [answers 
            (list 
	     (binary->natural '())
	     (binary->natural '(0 0 1))
	     (binary->natural '(0 0 1 1))
	     (binary->natural '(1 1 1 1))
	     (binary->natural '(1 0 1 0 1))
	     (binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1))
	     )])
      (display-results correct answers equal?)))

(define (test-set-difference )
    (let ([correct '(
		     (3 1 5)
		     ()
		     )]
          [answers 
            (list 
	     (set-difference '(1 2 3 4 5) '(2 4 6 8))
	     (set-difference '(5) '(5 6 7 8 9))
	     )])
      (display-results correct answers sequal?-grading)))

(define (test-walk-symbol)
    (let ([correct '(
		     5
		     b
		     c
		     5
		     5
		     ((c . a))
		     5
		     f
		     )]
          [answers 
            (list 
	     (walk-symbol 'a '((a . 5)))
	     (walk-symbol 'b '((a . 5)))
	     (walk-symbol 'a '((b . c) (a . b)))
	     (walk-symbol 'a '((a . 5) (b . 6) (c . a)))
	     (walk-symbol 'c '((a . 5) (b . (a . c)) (c . a)))
	     (walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a)))
	     (walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e)))
	     (walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e)))
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
  (display 'extend) 
  (test-extend)
  (display 'takef) 
  (test-takef)
  (display 'add-between) 
  (test-add-between)
  (display 'binary->natural) 
  (test-binary->natural)    
  (display 'set-difference ) 
  (test-set-difference )
  (display 'walk-symbol) 
  (test-walk-symbol)  
)

(define r run-all)

