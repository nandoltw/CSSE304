; CSSE304-01 Yuankai Wang (Kevin) Assignment7b
; Problem 6
(define map-by-position 
	(lambda (fn-list arg-list)
		(map 
			(lambda (a b)
				(a b))
			fn-list arg-list)))

; Problem 7
(define bt-leaf-sum
	(lambda (T)
		(bt-leaf-sum-node T)))

(define bt-leaf-sum-node
	(lambda (T)
		(if (list? T)
			(+ (bt-leaf-sum-node (cadr T)) (bt-leaf-sum-node (caddr T)))
			T)))

(define bt-inorder-list
	(lambda (T)
		(bt-inorder-list-node T)))

(define bt-inorder-list-node
	(lambda (T)
		(if (list? T)
			(if (or (list? (cadr T)) (list? (caddr T)))
				(append (bt-inorder-list-node (cadr T)) (list (car T)) (bt-inorder-list-node (caddr T)))
				(list (car T)))
			'())))

(define bt-max
	(lambda (T)
		(bt-max-node T)))

(define bt-max-node
	(lambda (T)
		(if (list? T)
			(max (bt-max-node (cadr T)) (bt-max-node (caddr T)))
			T)))

(define bt-max-interior
	(lambda (T)
		(third (bt-max-interior-fourcases T))))

(define first
	(lambda (list)
		(car list)))

(define second 			;left
	(lambda (list) 
		(cadr list)))

(define third			;right
	(lambda (list) 
		(caddr list)))

(define bt-max-interior-fourcases
	(lambda (T)
		(cond [(and (number? (second T)) (number? (third T)))
				(list (+ (second T) (third T)) (+ (second T) (third T)) (first T))]; A list that first element is the largest sum, second element is the sum under current letter, third element is the largest letter
			  [(number? (second T))
				(let* ([right (bt-max-interior-fourcases (third T))] [the-max (max (first right) (+ (second right) (second T)))])
					(if (equal? the-max (first right))
						(list (first right) (+ (second right) (second T)) (third right))
						(list (+ (second right) (second T)) (+ (second right) (second T)) (first T))))]
		      [(number? (third T))
				(let* ([left (bt-max-interior-fourcases (second T))] [the-max (max (first left) (+ (second left) (third T)))])
					(if (equal? the-max (first left))
						(list (first left) (+ (second left) (third T)) (third left))
						(list (+ (second left) (third T)) (+ (second left) (third T)) (first T))))]
			  [else (let* ([left (bt-max-interior-fourcases (second T))]
			  				[right (bt-max-interior-fourcases (third T))]
			  				[the-max (max (first left) (first right) (+ (second right) (second left)))])
					(cond [(equal? the-max (first left))
							(list (first left) (+ (second right) (second left)) (third left))]
						  [(equal? the-max (first right))
							(list (first right) (+ (second right) (second left)) (third right))]
						  [else (list (+ (second right) (second left)) (+ (second right) (second left)) (first T))]))])))