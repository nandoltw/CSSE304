; CSSE304-01 Yuankai Wang (Kevin) Assignment5
; Problem 1 
(define minimize-interval-list
	(lambda (ls)
		(combine-interval (list-sort < (car (apply map list ls)))
			(list-sort < (cadr (apply map list ls)))
			(car (list-sort < (car (apply map list ls))))
			'())))

(define combine-interval
	(lambda (ls1 ls2 small output)
		(if (null? (cdr ls2))
			(append output (list (list small (car ls2))))
			(if (<= (cadr ls1) (car ls2))
				(combine-interval (cdr ls1) (cdr ls2) small output)
				(append (list (list small (car ls2)))
					(combine-interval (cdr ls1) (cdr ls2) (cadr ls1) output))))))

; Problem 2
(define exists?
	(lambda (pred ls)
		(if (null? ls)
			#f
			(if (pred (car ls))
				#t
				(exists? pred (cdr ls))))))

; Problem 3
(define list-index 
	(lambda (pred ls)
		(list-index-count pred ls 0)))

(define list-index-count
	(lambda (pred ls count)
		(if (null? ls)
			#f
			(if (pred (car ls))
				count
				(list-index-count pred (cdr ls) (+ count 1))))))

; Problem 4
; Get from 0.ss
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

(define pascal-triangle
	(lambda (n)
		(if (< n 0)
			'()
			(pascal-triangle-all n 0 '()))))

(define pascal-triangle-all
	(lambda (n count output)
		(if (< n count)
			output
			(pascal-triangle-all n (+ count 1) (cons (pascal-triangle-one-line count 0) output)))))

(define pascal-triangle-one-line
	(lambda (n k)
		(if (= k n)
			(list 1)
			(cons (/ (fact n) (* (fact k) (fact (- n k)))) (pascal-triangle-one-line n (+ k 1))))))

; Problem 5
(define product
	(lambda (set1 set2)
		(if (null? set1)
			'()
			(append (n-list-product (car set1) set2 '()) (product (cdr set1) set2)))))

(define n-list-product
	(lambda (n set output)
		(if (null? set)
			'()
			(append (list (list n (car set))) (n-list-product n (cdr set) output)))))

; Problem 6
(define max-edges
	(lambda (n)
		(if (zero? n)
			0
			(/ (* n (- n 1)) 2))))

; Problem 7
(define complete?
	(lambda (G)
		(complete?-l G (length G))))

(define complete?-l
	(lambda (G l)
		(if (null? G)
			#t
			(if (null? (cadar G))
				#t
				(if (equal? l (+ 1 (length (cadar G))))
					(complete?-l (cdr G) l)
					#f)))))

; Problem 8
(define complete
	(lambda (ls)
		(if (null? ls)
			'()
			(if (null? (cdr ls))
				(list (list (car ls) '()))
				(complete-ls ls (cdr ls) '())))))

(define complete-ls
	(lambda (ls lst output)
		(if (null? ls)
			output
			(complete-ls (cdr ls) (cdr (append lst (list (car ls)))) (append output (list (list (car ls) lst)))))))

; Problem 9
(define replace
	(lambda (old new ls)
		(replace-output old new ls '())))

(define replace-output
	(lambda (old new ls output)
		(if (null? ls)
			(reverse output)
			(if (equal? old (car ls))
				(replace-output old new (cdr ls) (cons new output))
				(replace-output old new (cdr ls) (cons (car ls) output))))))

; Problem 10
(define remove-first
	(lambda (element ls)
		(remove-first-output element ls '() 0)))

(define remove-first-output
	(lambda (element ls output count)
		(if (null? ls)
			(reverse output)
			(if (and (equal? element (car ls)) (zero? count))
				(remove-first-output element (cdr ls) output (+ count 1))
				(remove-first-output element (cdr ls) (cons (car ls) output) count)))))

; Problem 11
(define remove-last
	(lambda (element ls)
		(reverse (remove-first-output element (reverse ls) '() 0))))