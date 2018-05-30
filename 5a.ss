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