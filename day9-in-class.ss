(define a (list + *))

(define aa '(1 2 3 . 4))
(define bb (cons (car aa) (cdr aa)))
(set-car! aa 5)
(set-car! (cdr aa) 6)
aa
bb