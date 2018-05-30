; CSSE304-01 Yuankai Wang (Kevin) Assignment5b
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