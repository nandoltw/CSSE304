;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

(load "chez-init.ss") 

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

; parsed expression

(define-datatype expression expression?  
    [var-exp
        (id symbol?)] 
    [lambda-exp
        (id (list-of symbol?))
        (body (list-of expression?))]
    [prim-exp
        (id symbol?)
        (body (list-of expression?))]
    [lit-exp
      (datum
          (lambda (x)
            (ormap
                (lambda (pred) (pred x))
                (list number? vector? boolean? symbol? string? pair? null?))))]   
    [if-exp
        (condition expression?)
        (tcase expression?)
        (other expression?)]
    [let-exp
        (syms (list-of symbol?))
        (exps (list-of expression?))
        (bodies (list-of expression?))]
    [app-exp
        (rator expression?)
        (rand (list-of expression?))])

	

;; environment type definitions

(define scheme-value?
    (lambda (x) #t))

(define-datatype environment environment?
    (empty-env-record)
    (extended-env-record
        (syms (list-of symbol?))
        (vals (list-of scheme-value?))
        (env environment?)))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
    [prim-proc
        (name symbol?)]
    [closure
        (syms (list-of symbol?))
        (bodies (list-of expression?))
        (env environment?)])
	 


;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+


; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp         
    (lambda (datum)
        (cond
            [(null? datum) datum]
            [(number? datum) (lit-exp datum)]
            [(boolean? datum) (lit-exp datum)]
            [(string? datum) (lit-exp datum)]
            [(vector? datum) (lit-exp datum)]
            [(symbol? datum) (var-exp datum)]
            [(pair? datum)
                (cond
                    [(eqv? (car datum) 'quote)
                        (lit-exp `,(2nd datum))];get help from Jizhou Huang on `
                    [(eqv? (car datum) 'lambda)
                        (lambda-exp (2nd datum)
                            (map parse-exp (cddr datum)))]
                    [(eqv? (car datum) 'set!)
                       (set!-exp (2nd datum)
                            (parse-exp (3rd datum)))]
                    [(eqv? (car datum) 'if)
                        (if-exp (parse-exp (2nd datum))
                                (parse-exp (3rd datum))
                                (parse-exp (cadddr datum)))]
                    [(equal? (1st datum) 'let)
                        (let-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum)))]
                    [else (app-exp (parse-exp (1st datum))
        	            (map parse-exp (cdr datum)))])]
            [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))








;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+





; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and 2.3

(define empty-env
    (lambda ()
        (empty-env-record)))

(define extend-env
    (lambda (syms vals env)
        (extended-env-record syms vals env)))

(define list-find-position
    (lambda (sym los)
        (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
    (lambda (pred ls)
        (cond
            ((null? ls) #f)
            ((pred (car ls)) 0)
            (else (let ((list-index-r (list-index pred (cdr ls))))
	               (if  (number? list-index-r)
		                (+ 1 list-index-r)
		                #f))))))

(define apply-env
    (lambda (env sym succeed fail) ; succeed and fail are "callback procedures, 
        (cases environment env       ;  succeed is appluied if sym is found, otherwise 
            [empty-env-record ()       ;  fail is applied.
                (fail)]
            [extended-env-record (syms vals env)
		        (let ((pos (list-find-position sym syms)))
      	             (if (number? pos)
				         (succeed (list-ref vals pos))
				         (apply-env env sym succeed fail)))])))








;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+



; To be added later









;-------------------+
;                   |
;   INTERPRETER    |
;                   |
;-------------------+



; top-level-eval evaluates a form in the global environment

(define top-level-eval
    (lambda (form)
    ; later we may add things that are not expressions.
        (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter

(define eval-exp
    (lambda (exp env)
    (cases expression exp
        [lit-exp (datum) datum]
        [var-exp (id)
			(apply-env env
                        id; look up its value.
                        (lambda (x) x) ; procedure to call if id is in the environment 
                        (lambda ()
                            (apply-env init-env
                                       id
                                       (lambda (x) x)
                                       (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
                                            "variable not found in environment: ~s" id)))))]
        [app-exp (rator rands)
            (let ([proc-value (eval-exp rator env)]
                  [args (eval-rands rands env)])
            (apply-proc proc-value args))]
        [if-exp (condition tcase other)
            (if (eval-exp condition env)
                (eval-exp tcase env)
                (eval-exp other env))]
        [lambda-exp (id body)
            (closure id body env)]
        [prim-exp (id body)
            (apply-prim-proc id (map (lambda (x) (eval-exp x env)) body))]
        [let-exp (syms exps bodies)
            (let ([extended-env
                (extend-env syms
                    (map (lambda (x) (eval-exp x env)) exps) env)])
            (eval-bodies bodies extended-env))]
        [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
    (lambda (rands env)
        (map (lambda (e)
            (eval-exp e env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
    (lambda (proc-value args)
        (cases proc-val proc-value
            [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
            [closure (ids bodies env)
                (eval-bodies bodies
                            (extend-env ids
                                        args
                                        env))]
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define eval-bodies
    (lambda (bodies env)
        (if (null? (cdr bodies))
            (eval-exp (car bodies) env)
            (begin (eval-exp (car bodies) env)
                   (eval-bodies (cdr bodies) env)))))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < > <= >= cons car cdr list null? 
                            assq eq? equal? atom? length list->vector list? pair? procedure?
                            vector->list vector make-vector vector-ref vector? number? symbol? 
                            set-car! set-cdr! vector-set! display newline cadr caar cdar cadar))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
    (lambda (prim-proc args)
        (case prim-proc
            [(+) (apply + args)]
            [(-) (apply - args)]
            [(*) (apply * args)]
            [(/) (apply / args)]
            [(add1) (+ (1st args) 1)]
            [(sub1) (- (1st args) 1)]
            [(zero?) (zero? (1st args))]
            [(not) (not (1st args))]
            [(=) (= (1st args) (2nd args))]
            [(<) (< (1st args) (2nd args))]
            [(>) (> (1st args) (2nd args))]
            [(<=) (<= (1st args) (2nd args))]
            [(>=) (>= (1st args) (2nd args))]
            [(cons) (cons (1st args) (2nd args))]
            [(car) (car (1st args))]
            [(cdr) (cdr (1st args))]
            [(list) args]
            [(null?) (null? (1st args))]
            [(assq) (assq (1st args) (2nd args))]
            [(eq?) (eq? (1st args) (2nd args))]
            [(equal?) (equal? (1st args) (2nd args))]
            [(atom?) (atom? (1st args))]
            [(length) (length (1st args))]
            [(list->vector) (list->vector (1st args))]
            [(list?) (list? (1st args))]
            [(pair?) (pair? (1st args))]
            [(procedure?) (proc-val? (1st args))]
            [(vector->list) (vector->list (1st args))]
            [(vector) (vector (1st args))]
            [(make-vector) (make-vector (1st args))]
            [(vector-ref) (vector-ref (1st args) (2nd args))]
            [(vector?) (vector? (1st args))]
            [(number?) (number? (1st args))]
            [(symbol?) (symbol? (1st args))]
            [(set-car!) (set-car! (1st args) (2nd args))]
            [(set-cdr!) (set-cdr! (1st args) (2nd args))]
            [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
            [(display) (display (1st args))]
            [(newline) (newline)]
            [(cadr) (cadr (1st args))]
            [(caar) (caar (1st args))]
            [(cdar) (cdar (1st args))]
            [(cadar) (cadar (1st args))]
            [else (error 'apply-prim-proc 
                "Bad primitive procedure name: ~s" 
                prim-proc)])))

(define rep      ; "read-eval-print" loop.
    (lambda ()
        (display "--> ")
    ;; notice that we don't save changes to the environment...
        (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
            (eopl:pretty-print answer) (newline)
            (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))