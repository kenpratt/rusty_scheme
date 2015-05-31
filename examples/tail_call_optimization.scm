(define (tco-test)
  (define (f i)
    (if (= i 100000)
        (displayln "made it to 100000")
        (f (+ i 1))))
  (f 1)
  (displayln "done"))

(tco-test)
