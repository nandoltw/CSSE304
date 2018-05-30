(define pi (* 4 (atan 1)))

(define circle-area
  (lambda (radius)
    (* pi radius radius)))

(define washer-area
  (lambda (outer-radius inner-radius)
     (- (circle-area outer-radius)        
	(circle-area inner-radius))))


