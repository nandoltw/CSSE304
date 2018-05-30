; CSSE304-01 Yuankai Wang (Kevin) Assignment7a
; Problem 1 
(define vector-append-list
	(lambda (vec ls)
		(let ([new-vector
				(make-vector (+ (vector-length vec) (length ls)))])
				(copy-from-vector new-vector vec 0)
				(copy-from-list new-vector ls (vector-length vec))
			new-vector)))

(define copy-from-vector
	(lambda (new-vector vec count)
		(if (equal? count (vector-length vec))
			new-vector
			(begin (vector-set! new-vector count (vector-ref vec count))
				(copy-from-vector new-vector vec (+ count 1))))))

(define copy-from-list
	(lambda (new-vector ls count)
		(if (null? ls)
			new-vector
			(begin (vector-set! new-vector count (car ls))
				(copy-from-list new-vector (cdr ls) (+ count 1))))))

; Problem 2
(define qsort
	(lambda (pred ls)
		(if (null? ls)
			'()
			(quicksort pred (cdr ls) (car ls)))))

(define quicksort
	(lambda (pred ls pivot)
		(if (null? ls)
			(list pivot)
			(if (< (length ls) 2)
				(if (pred (car ls) pivot)
					(list (car ls) pivot)
					(list pivot (car ls)))
				(append (qsort pred (filter (lambda (num) (pred num pivot)) ls))
						(list pivot)
						(qsort pred (filter-out (lambda (num) (pred num pivot)) ls))))))) 
						;Get help on the lambda thing from Xiangbei Chen

; Copy from homework6
(define filter-out
	(lambda (pred? lst)
		(filter-out-output pred? lst '())))

(define filter-out-output
	(lambda (pred ls output)
		(if (null? ls)
			output
			(if (pred (car ls))
				(filter-out-output pred (cdr ls) output)
				(filter-out-output pred (cdr ls) (append output (list (car ls))))))))

; Problem 3
(define connected?
	(lambda (g)
		(if (or (null? g) (equal? (length g) 1))
			#t
			(list-equal (get-vertex-list g) (union-list (cdr (graph->list g '()))
														(union-list (cdr (graph->list g '()))
																	(car (graph->list g '()))))))))

(define get-vertex-list
	(lambda (g)
		(if (null? g)
			'()
			(cons (caar g) (get-vertex-list (cdr g))))))

(define graph->list
	(lambda (g lst)
		(if (null? g)
			lst
			(graph->list (cdr g) (append (list (cons (caar g) (cadar g))) lst)))))

(define union-list
	(lambda (lst union-lst)
		(if (null? lst)
			union-lst
			(if (< (+ (length union-lst) (length (car lst))) (length (union union-lst (car lst))))
				(union-list (cdr lst) (union union-lst (car lst)))
				(union-list (cdr lst) union-lst)))))

(define list-equal
	(lambda (l1 l2)
		(if (null? l1)
			#t
			(if (member (car l1) l2)
				(list-equal (cdr l1) l2)
				#f))))

; Copy from homework 3
(define union
	(lambda (s1 s2)
		(if (null? s2)
			s1
			(if (duplicate? (car s2) s1)
				(union s1 (cdr s2))
				(union (cons (car s2) s1) (cdr s2))))))

(define duplicate?
	(lambda (n list)
		(if (null? list)
			#f
			(if (equal? n (car list))
				#t
				(if (null? (cdr list))
					#f
					(duplicate? n (cdr list)))))))

; Problem 4
(define reverse-it
	(lambda (lst)
		(reverse-length lst '())))

(define reverse-length
	(lambda (lst output)
		(if (null? lst)
			output
			(reverse-length (cdr lst) (cons (car lst) output)))))

; Problem 5
(define empty-BST
	(lambda ()
		'()))

(define empty-BST?
	(lambda (obj)
		(null? obj)))

(define BST-insert
	(lambda (num bst)
		(node-insert num bst)))

(define node-insert
	(lambda (num bst)
		(cond [(null? bst)
				(list num (empty-BST) (empty-BST))]
			  [(< num (car bst))
			    (list (car bst) (node-insert num (cadr bst)) (caddr bst))]
			  [(> num (car bst))
			    (list (car bst) (cadr bst) (node-insert num (caddr bst)))]
			  [else (= num (car bst))
			    bst])))

(define BST-inorder
	(lambda (bst)
		(node-inorder bst)))

(define node-inorder
	(lambda (bst)
		(if (null? bst)
			'()
			(append (node-inorder (cadr bst)) (list (car bst)) (node-inorder (caddr bst))))))

(define BST?
	(lambda (obj)
		(if (list? obj)
			(if (null? obj)
				#t
				(if (equal? (length obj) 3)
					(if (number? (car obj))
						(if (and (list? (cadr obj)) (list? (caddr obj)))
							(if (equal? (node-inorder obj) (sort < (node-inorder obj)))
								(and (BST? (cadr obj)) (BST? (caddr obj)))
								#f)
							#f)
						#f)
					#f))
			#f)))

(define BST-element
	(lambda (bst)
		(car bst)))

(define BST-left 
	(lambda(bst)
		(cadr bst)))

(define BST-right
	(lambda (bst)
		(caddr bst)))

(define BST-insert-nodes
	(lambda (bst nums)
		(BST-nodes bst nums)))

(define BST-nodes
	(lambda (bst nums)
		(if (null? nums)
			bst
			(BST-nodes (BST-insert (car nums) bst) (cdr nums)))))

(define BST-contains?
	(lambda (bst num)
		(bst-contains-node bst num)))

(define bst-contains-node
	(lambda (bst num)
		(if (null? bst)
			#f
			(cond [(equal? (car bst) num)
					#t]
				  [(< num (car bst)) (bst-contains-node (BST-left bst) num)]
				  [(> num (car bst)) (bst-contains-node (BST-right bst) num)]))))

(define BST-height
	(lambda (bst)
		(bst-height-node bst)))

(define bst-height-node
	(lambda (bst)
		(if (null? bst)
			-1
			(+ 1 (max (bst-height-node (cadr bst)) (bst-height-node (caddr bst)))))))