(define make-queue
  (lambda ()
    (cons '() '())))

(define empty-queue?
  (lambda (q)
    (eq? (car q) '())))

(define enqueue!
  (lambda (obj q)
    (let ((x (cons obj '())))
      (if (null? (car q))
          (set-car! q x)
          (set-cdr! (cdr q) x))
      (set-cdr! q x))))

(define dequeue!
  (lambda (q)
    (if (null? (car q))
        (error 'dequeue! "cannot dequeue from empty queue")
        (let ((obj (caar q)))
          (set-car! q (cdar q))
          (if (null? (car q))
              (set-cdr! q '()))
          obj))))


;;; > (define q (make-queue))
;;; > q
;;; (())
;;; > (enqueue! 'a q)
;;; > q
;;; ((a) a)
;;; > (enqueue! 'b q)
;;; > (enqueue! 'c q)
;;; > q
;;; ((a b c) c)
;;; > (dequeue! q)
;;; a
;;; > q
;;; ((b c) c)
;;; > (dequeue! q)
;;; b
;;; > (dequeue! q)
;;; c
;;; > (dequeue! q)
;;; 
;;; Error in dequeue!: cannot dequeue from empty queue.
;;; Type (debug) to enter the debugger.
;;; > 
