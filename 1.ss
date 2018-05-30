(define (Fahrenheit->Celsius temperature) 
  (/ (- temperature 32) 9/5))
(define (interval-contains? interval number)
  (cond 
    [(< number (car interval)) #f]
    [(> number (cadr interval)) #f]
    [else #t]))
(define (interval-intersects? i1 i2)
  (if (< (cadr i1) (car i2)) #f 
      (if (> (car i1) (cadr i2)) #f 
          #t)))
(define (interval-union i1 i2) 
  (if (interval-intersects? i1 i2) 
      (list (list (min (car i1) (car i2)) (max (cadr i1) (cadr i2))))
      (list i1 i2)))
(define (divisible-by-7? num)
  (if (zero? num) #t
      (if (< num 7) #f 
          (divisible-by-7? (- num 7)))))
(define (ends-with-7? num)
  (if (equal? 7 (modulo num 10)) #t #f))
(define (first list)
  (car list))
(define (second list)
  (cadr list))
(define (third list)
  (caddr list))