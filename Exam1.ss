; CSSE304-01 Yuankai Wang (Kevin) Exam1
; Problem 1
(define extend
	(lambda (x p)
		(lambda (obj)
			(or (eq? x obj) (p obj)))))

; Problem 2
(define takef
	(lambda (p ls)
		(if (p (car ls))
			(cons (car ls) (takef p (cdr ls)))
			'())))

; Problem 3
(define add-between
	(lambda (obj ls)
		(if (null? ls)
			'()
			(if (null? (cdr ls))
				(list (car ls))
				(append (list (car ls) obj) (add-between obj (cdr ls)))))))

; Problem 4
(define binary->natural
	(lambda (lon)
		(binary->natural-coeffient lon 1)))

(define binary->natural-coeffient
	(lambda (lon coef)
		(if (null? lon)
			0
			(+ (* (car lon) coef) (binary->natural-coeffient (cdr lon) (* coef 2))))))

; Problem 5
(define set-difference
	(lambda (s1 s2)
		(differ s1 s2 '())))

(define differ
	(lambda (s1 s2 output)
		(if (null? s1)
			output
			(if (member (car s1) s2)
				(differ (cdr s1) s2 output)
				(differ (cdr s1) s2 (cons (car s1) output))))))

; Problem 6
(define walk-symbol
	(lambda (sym . a-list)
		(walk-symbol-rec sym (car a-list))))

(define walk-symbol-list
	(lambda (sym alist)
		(if (null? alist)
			sym
			(if (equal? sym (car (car alist)))
				(cdr (car alist))
				(walk-symbol-list sym (cdr alist))))))

(define walk-symbol-rec
	(lambda (sym alist)
		(if (and (symbol? sym) (check-in-list sym alist))
			(walk-symbol-rec (walk-symbol-list sym alist) alist)
			sym)))

(define check-in-list
	(lambda (sym list)
		(if (null? list)
			#f
			(if (equal? sym (car (car list)))
				#t
				(check-in-list sym (cdr list))))))