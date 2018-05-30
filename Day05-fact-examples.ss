;; Original version of factorial function:

(define fact     ; standard factorial function.
  (lambda (n)
    (if (zero? n)
            1
            (* n (fact (- n 1))))))
			
			
(define fact-tail   ; tail-recursive version with accumulator
   
   (lambda (n accum)
      (if (zero? n)
           accum
          (fact-tail (- n 1) (* n accum)))))
(define factorial
    (lambda (n)
    (fact-tail n 1))

;; Now an Experiment.  For each expression, try to predict what will happen.
;; Then execute it to see what really happens.  Can you explain what you see?
(define f fact)

(f 5)

(define fact
   (lambda (n) 
      "Abe Lincoln elected President"))

(fact 1860)

(f 1860)


; We'd like to write fact so that we can rename it safely:
;   (defnie g fact)
;   (define fact whatever) and still have g work.
; It would also be nice to add the efficiency-enhancing acccumulator

; How about this?

(define fact
  (let ([fact-real (lambda (n accum)
                     (if (zero? n)
                         accum
                         (fact-real (- n 1) (* n accum))))])
    (lambda (n) (fact-real n 1))))



; Solution?

(define fact 
  (letrec ([fact-tail 
            (lambda (n prod)
              (if (zero? n)
                  prod
                  (fact-tail (sub1 n) 
                         (* n prod))))])
    (lambda (n)  (fact-tail n 1))))



; another letrec example

(define odd?
  (letrec ([odd? (lambda (n) 
		   (if (zero? n) 
		       #f 
		       (even? (sub1 n))))]
           [even? (lambda (n) 
                     (if (zero? n) 
                         #t 
                         (odd? (sub1 n))))])
    (lambda (n)
      (odd? n))))

;; Named let version of fact

(define fact
  (lambda (n)
    (let loop ([x n] [prod 1])
      (if (zero? x)
	  prod
	  (loop (sub1 x) (* prod x))))))
