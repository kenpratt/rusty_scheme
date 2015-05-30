;; FIFO queue.
(define thread-pool '())

;; Push to end of queue.
(define (push-thread t)
  (set! thread-pool (append thread-pool (list t))))

;; Pop from front of queue.
(define (pop-thread)
  (if (null? thread-pool)
      '()
      (let ((t (car thread-pool)))
        (set! thread-pool (cdr thread-pool))
        t)))

;; To start, set the exit function to the point.
(define (start)
  (call/cc
   (lambda (cc)
     (set! exit cc)
     (run-next-thread))))

;; Exit point will be defined when start is run.
(define exit '())

;; Run the next thread in line. If no more, call exit.
(define (run-next-thread)
  (let ((t (pop-thread)))
    (if (null? t)
        (exit)
        (t))))

;; Create a new thread
(define (spawn fn)
  (push-thread
   (lambda ()
     (fn)
     (run-next-thread))))

;; Yield saves the running state of the current thread,
;; and then runs the next one.
(define (yield)
  (call/cc
   (lambda (cc)
     (push-thread cc)
     (run-next-thread))))

(spawn
 (lambda ()
   (displayln "Hello from thread #1")
   (yield)
   (displayln "Hello again from thread #1")
   (yield)
   (displayln "Hello once more from thread #1")))

(spawn
 (lambda ()
   (displayln "Hello from thread #2")
   (yield)
   (displayln "Hello again from thread #2")
   (yield)
   (displayln "Hello once more from thread #2")))

(displayln "Starting...")
(start)
(displayln "Done")
