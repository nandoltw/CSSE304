; CSSE304-01 Yuankai Wang (Kevin) Assignment10
; Problem 1 
(define free-vars
	(lambda (e)
		(no-duplicate (free-vars-output e '()) '())))

(define free-vars-output
	(lambda (e output)
		(cond [(null? e) output]
          [(symbol? e) (cons e output)]
          [(equal? (car e) 'lambda) (if (list? (caddr e))
										                    (let ([next (free-vars-output (caddr e) '())])
                                          (append (filter (lambda (x) (not (equal? (caadr e) x))) next) output))
                                          (if (not (member (caddr e) (cadr e)))
                                            (cons (caddr e) output)
                                            output))]
          [else (append (free-vars-output (car e) '()) (free-vars-output (cdr e) '()) output)])))

(define no-duplicate
  (lambda (lst output)
    (if (null? lst)
      output
      (if (member (car lst) output)
        (no-duplicate (cdr lst) output)
        (no-duplicate (cdr lst) (cons (car lst) output))))))

(define bound-vars
  (lambda (e)
    (no-duplicate (bound-vars-output e '()) '())))

(define bound-vars-output
  (lambda (e output)
    (cond [(null? e) output]
          [(symbol? e) output]
          [(equal? (car e) 'lambda) (if (list? (caddr e))
                                        (let ([next (bound-vars-output (caddr e) '())])
                                          (if (null? next)
                                            (let ([next2 (free-vars-output (caddr e) '())])
                                              (if (and (not (null? next2)) (equal? (car next2) (caadr e)))
                                                (cons (car next2) output)
                                                output))
                                            (append (bound-vars (list (car e) (cadr e) (car next))) output)))
                                        (if (and (equal? (caddr e) (caadr e)) (not (null? (free-vars (caddr e)))))
                                           (cons (caddr e) output)
                                           output))]
          [else (append (bound-vars-output (car e) '()) (bound-vars-output (cdr e) '()) output)])))

; Problem 2
; Copy from HW6
(define let->application
  (lambda (proc)
    (if (null? (cadr proc))
      (list (append (list 'lambda) (cdr proc)))
      (append (list (append (list 'lambda (car (apply map list (cadr proc)))) (cddr proc))) (cadr (apply map list (cadr proc)))))))

(define let*->let
  (lambda (proc)
    (let-helper (cadr proc) proc)))

(define let-helper
  (lambda (list1 proc)
    (if (null? (cdr list1))
      (list 'let (list (car list1)) (caddr proc))
      (list 'let (list (car list1)) (let-helper (cdr list1) proc)))))

(define occurs-free?
  (lambda (var exp)
    (cond
      [(null? exp) #f]
      [(symbol? exp) (eqv? var exp)]
      [(eqv? (car exp) 'lambda)
             (and (not (member var (cadr exp))) (occurs-free? var (caddr exp)))]
      [(eqv? (car exp) 'if)
             (or (or (occurs-free? var (cadr exp)) (occurs-free? var (caddr exp))) (occurs-free? var (cadddr exp)))]
      [(eqv? (car exp) 'let)
             (occurs-free? var (let->application exp))]
      [(eqv? (car exp) 'let*)
             (occurs-free? var (let*->let exp))]
      [(eqv? (car exp) 'set!)
             (occurs-free? var (caddr exp))]
      [else (or (occurs-free? var (car exp))
                (occurs-free? var (cdr exp)))])))

(define occurs-bound?
  (lambda (var exp)
    (cond
      [(null? exp) #f]
      [(symbol? exp) #f]
      [(eqv? (car exp) 'lambda)
             (or (occurs-bound? var (caddr exp))
                 (and (member var (cadr exp))
                      (occurs-free? var (caddr exp))))]
      [(eqv? (car exp) 'if)
             (or (or (occurs-bound? var (cadr exp)) (occurs-bound? var (caddr exp))) (occurs-bound? var (cadddr exp)))]
      [(eqv? (car exp) 'let)
             (occurs-bound? var (let->application exp))]
      [(eqv? (car exp) 'let*)
             (occurs-bound? var (let*->let exp))]
      [(eqv? (car exp) 'set!)
             (occurs-bound? var (caddr exp))]
      [else (or (occurs-bound? var (car exp))
                (occurs-bound? var (cdr exp)))])))

; Problem 3
(define lexical-address
  (lambda (exp)
    (lexical-address-stack exp '())))

(define lexical-address-stack
  (lambda (exp stack)
    (cond [(null? exp) '()]
          [(symbol? exp) (cons ': (find-depth-pos exp stack 0))]
          [(null? (car exp)) (lexical-address-stack (cdr exp) stack)]
          [(equal? (car exp) 'lambda)
              (append (list 'lambda (cadr exp))
                      (lexical-address-stack (cddr exp) (cons (cadr exp) stack)))]
          [(equal? (car exp) 'if)
              (cons 'if (lexical-address-stack (cdr exp) stack))]
          [(equal? (car exp) 'set!)
              (append (list 'set! (cadr exp)) (lexical-address-stack (cddr exp) stack))]
          [(equal? (car exp) 'let)
              (append (list 'let (map (lambda (x) (list (car x) (lexical-address-stack (cadr x) stack))) (cadr exp)))
                (lexical-address-stack (cddr exp) (cons (map car (cadr exp)) stack)))]
          [else (cons (lexical-address-stack (car exp) stack) 
                      (lexical-address-stack (cdr exp) stack))])))

(define find-depth-pos
  (lambda (var s depth)
    (cond [(null? s) (list 'free var)]
          [(member var (car s)) (list depth (find-pos var (car s)))]
          [else (find-depth-pos var (cdr s) (+ depth 1))])))

(define find-pos
  (lambda (var lst)
    (if (equal? var (car lst))
        0
        (+ 1 (find-pos var (cdr lst))))))

; Problem 4
(define un-lexical-address
  (lambda (exp)
    (unlexical-address-stack exp '())))

(define unlexical-address-stack
  (lambda (exp stack)
    (cond [(null? exp) '()]
          [(null? (car exp)) (unlexical-address-stack (cdr exp) stack)]
          [(equal? (car exp) ':)
              (unlexical exp stack 0)]
          [(equal? (car exp) 'lambda)
              (append (list 'lambda (cadr exp))
                      (unlexical-address-stack (cddr exp) (cons (cadr exp) stack)))]
          [(equal? (car exp) 'if)
              (cons 'if (unlexical-address-stack (cdr exp) stack))]
          [(equal? (car exp) 'set!)
              (append (list 'set! (cadr exp)) (unlexical-address-stack (cddr exp) stack))]
          [(equal? (car exp) 'let)
              (append (list 'let (map (lambda (x) (list (car x) (unlexical-address-stack (cadr x) stack))) (cadr exp)))
                (unlexical-address-stack (cddr exp) (cons (map car (cadr exp)) stack)))]
          [else (cons (unlexical-address-stack (car exp) stack) 
                      (unlexical-address-stack (cdr exp) stack))])))

(define unlexical
  (lambda (exp stack count-depth)
    (cond [(equal? (cadr exp) 'free)
              (caddr exp)]
          [(equal? (cadr exp) count-depth)
              (un-pos (caddr exp) (car stack) 0)]
          [else (unlexical exp (cdr stack) (+ 1 count-depth))])))

(define un-pos
  (lambda (pos lst count-pos)
    (if (equal? pos count-pos)
        (car lst)
        (un-pos pos (cdr lst) (+ 1 count-pos)))))