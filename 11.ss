; CSSE304-01 Yuankai Wang (Kevin) Assignment11
(load "chez-init.ss")

(define-datatype bintree bintree?
    (leaf-node
        (num integer?))
    (interior-node
        (key symbol?)
        (left bintree?)
        (right bintree?)))
; Problem 1
(define-syntax my-let
    (syntax-rules ()
        [(let ((var expr) ...) body1 body2 ...)
            ((lambda (var ...) body1 body2 ...)
                expr ...)]
        [(_ loop ((var expr) ...) body1 body2 ...)
            (letrec ((loop (lambda (var ...) body1 body2 ...)))
                (loop expr ...))]))

(define-syntax my-or
    (syntax-rules ()
        [(_) #f]
        [(_ e) e]
        [(_ e1 e2 ...) (begin (let ((temp e1))
                                (if temp
                                       temp
                                       (my-or e2 ...))))]))

(define-syntax +=
    (syntax-rules ()
        [(_ e num)
            (begin (set! e (+ e num)) e)]))

(define-syntax return-first
    (syntax-rules ()
        [(_ e) e]
        [(_ e1 e2) e1]
        [(_ e1 e2 e3 ...) (let ((temp e1)) (begin (begin e2 e3 ...) temp))]))

; Problem 2
(define bintree-to-list
    (lambda (lst)
        (cases bintree lst
            [leaf-node (num) (list 'leaf-node num)]
            [interior-node (key left right) (list 'interior-node key (bintree-to-list left) (bintree-to-list right))]
            [else (eopl:error 'bintree-to-list "improper data format")])))

; Problem 3
(define max-interior
    (lambda (lst)
        (caddr (max-interior-cases lst))))

(define max-interior-cases
    (lambda (lst)
        (cases bintree lst
            [leaf-node (num) num]
            [interior-node (key left right) 
                (let ([l (max-interior-cases left)] [r (max-interior-cases right)])
                    (cond [(and (is-leaf-node left) (is-leaf-node right))
                                (list (+ l r) (+ l r) key)]
                          [(is-leaf-node right)
                                (let ([the-max (max (car l) (+ (cadr l) r))])
                                    (if (equal? the-max (car l))
                                        (list (car l) (+ (cadr l) r) (caddr l))
                                        (list (+ (cadr l) r) (+ (cadr l) r) key)))]
                          [(is-leaf-node left)
                                (let ([the-max (max (car r) (+ l (cadr r)))])
                                    (if (equal? the-max (car r))
                                        (list (car r) (+ (cadr r) l) (caddr r))
                                        (list (+ (cadr r) l) (+ (cadr r) l) key)))]
                          [else 
                                (let ([the-max (max (car l) (car r) (+ (cadr l) (cadr r)))])
                                    (cond [(equal? the-max (car l)) (list (car l) (+ (cadr l) (cadr r)) (caddr l))]
                                          [(equal? the-max (car r)) (list (car r) (+ (cadr l) (cadr r)) (caddr r))]
                                          [else (list (+ (cadr l) (cadr r)) (+ (cadr l) (cadr r)) key)]))]))])))

(define is-leaf-node
    (lambda (lst)
        (cases bintree lst
            [leaf-node (num) #t]
            [interior-node (key left right) #f])))

; Problem 4
(define-datatype expression expression?
    [var-exp
        (id symbol?)]
    [lit-exp
        (num number?)]
    [vector-exp
        (vec vector?)]
    [lambda-exp
        (id (lambda (n) 
                (or (symbol? n)
                    ((list-of symbol?) n))))
        (body (list-of expression?))]
    [set!-exp
        (id symbol?)
        (exp expression?)]
    [if-exp
        (cond expression?)
        (then expression?)
        (else expression?)]
    [let-exp
        (id list?)
        (exp (list-of expression?))]
    [let*-exp
        (id list?)
        (exp (list-of expression?))]
    [letrec-exp
        (id list?)
        (exp (list-of expression?))]
    [app-exp
        (rator expression?)
        (rand (list-of expression?))])

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp
    (lambda (datum)
        (cond
            [(symbol? datum) (var-exp datum)]
            [(number? datum) (lit-exp datum)]
            [(vector? datum) (vector-exp datum)]
            [(not (list? datum)) (eopl:error 'parse-exp "expression ~s is not a proper list" datum)]
            [(pair? datum)
                (cond
                    [(eqv? (car datum) 'lambda)
                        (lambda-parse datum)]
                    [(eqv? (car datum) 'set!)
                        (set!-parse datum)]
                    [(eqv? (car datum) 'if)
                        (if-parse datum)]
                    [(eqv? (car datum) 'let)
                        (let-parse datum)]
                    [(eqv? (car datum) 'let*)
                        (let*-parse datum)]
                    [(eqv? (car datum) 'letrec)
                        (letrec-parse datum)]
                    [else (app-exp (parse-exp (1st datum))
                            (map parse-exp (cdr datum)))])]
            [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define lambda-parse
    (lambda (datum)
        (cond [(< (length datum) 3)
                (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)]
              [(not (checksymbol (2nd datum)))
                (eopl:error 'parse-exp "lambda argument list: formals must be symbols: ~s" datum)]
              [else (lambda-exp (2nd datum)
                    (map parse-exp (cddr datum)))])))

(define set!-parse
    (lambda (datum)
        (cond [(< (length datum) 3)
                (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" datum)]
              [(> (length datum) 3)
                (eopl:error 'parse-exp "too many parts: ~s" datum)]
              [else (set!-exp (2nd datum)
                (parse-exp (3rd datum)))])))

(define if-parse
    (lambda (datum)
        (cond [(< (length datum) 3)
                (eopl:error 'parse-exp "if expression: should have (only) test, then, and else clauses: ~s" datum)]
              [(> (length datum) 4)
                (eopl:error 'parse-exp "too many parts: ~s" datum)]
              [else (if-exp (parse-exp (2nd datum))
                       (parse-exp (3rd datum))
                       (parse-exp (cadddr datum)))])))

(define let-parse
    (lambda (datum)
        (cond [(< (length datum) 3)
                (eopl:error 'parse-exp "let-expression has incorrect length let" datum)]
              [(not (list? (2nd datum)))
                (eopl:error 'parse-exp "let* declarations not a list")]
              [(not (checklist (2nd datum)))
                (eopl:error 'parse-exp "decls: not all proper lists: ~s" (2nd datum))]
              [else (let-exp (map parse-exp (2nd datum))
                (map parse-exp (cddr datum)))])))

(define let*-parse
    (lambda (datum)
        (cond [(< (length datum) 3)
                (eopl:error 'parse-exp "let*-expression has incorrect length let" datum)]
              [(not (list? (2nd datum)))
                (eopl:error 'parse-exp "let* declarations not a list")]
              [(not (checklist (2nd datum)))
                (eopl:error 'parse-exp "decls: not all proper lists: ~s" (2nd datum))]
              [else (let*-exp (map parse-exp (2nd datum))
                (map parse-exp (cddr datum)))])))

(define letrec-parse
    (lambda (datum)
        (cond [(< (length datum) 3)
                (eopl:error 'parse-exp "letrec-expression has incorrect length let" datum)]
              [(not (list? (2nd datum)))
                (eopl:error 'parse-exp "let* declarations not a list")]
              [(not (checklist (2nd datum)))
                (eopl:error 'parse-exp "decls: not all proper lists: ~s" (2nd datum))]
              [else (letrec-exp (map parse-exp (2nd datum))
                (map parse-exp (cddr datum)))])))

(define checksymbol
    (lambda (exp)
        (if (null? exp)
            #t
            (if (symbol? exp)
                #t
                (and (symbol? (car exp)) (checksymbol (cdr exp)))))))

(define checklist
    (lambda (exp)
        (if (null? exp)
            #t
            (if (and (list? exp) (list? (car exp)))
                (if (equal? (length (car exp)) 2)
                    (if (symbol? (caar exp))
                        (and (list? (car exp)) (checklist (cdr exp)))
                        (eopl:error 'parse-exp "decls: not all length 2: ~s" (car exp)))
                    (eopl:error 'parse-exp "decls: first members must be symbols: ~s" (caar exp)))
                (eopl:error 'parse-exp "not a proper list: ~s" (car exp))))))

(define unparse-exp
    (lambda (exp)
        (cases expression exp
            [var-exp (id) id]
            [lit-exp (num) num]
            [vector-exp (vec) vec]
            [lambda-exp (id body) (apply list 'lambda id (map unparse-exp body))]
            [set!-exp (id exp) (apply list 'set! id (unparse-exp exp))]
            [if-exp (cond then else) (list 'if (unparse-exp cond) (unparse-exp then) (unparse-exp else))]
            [let-exp (id exp) (apply list 'let (map unparse-exp id) (map unparse-exp exp))]
            [let*-exp (id exp) (apply list 'let* (map unparse-exp id) (map unparse-exp exp))]
            [letrec-exp (id exp) (apply list 'letrec (map unparse-exp id) (map unparse-exp exp))]
            [app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))])))

; An auxiliary procedure that could be helpful.
(define var-exp?
    (lambda (x)
        (cases expression x
            [var-exp (id) #t]
            [else #f])))