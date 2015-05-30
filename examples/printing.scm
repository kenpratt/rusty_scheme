(define (dump what s)
  (displayln what)
  (write s)
  (newline)
  (display s)
  (newline)
  (print s)
  (newline)
  (newline))

(dump "symbol" 'blah)
(dump "int" 23)
(dump "bool" #t)
(dump "string" "blah")
(dump "list" '(blah 23 #t "blah" (blah 23 #t "blah")))
(dump "proc" dump)

;;
;; Expected output:
;;
;; symbol
;; blah
;; blah
;; 'blah
;;
;; int
;; 23
;; 23
;; 23
;;
;; bool
;; #t
;; #t
;; #t
;;
;; string
;; "blah"
;; blah
;; "blah"
;;
;; list
;; (blah 23 #t "blah" (blah 23 #t "blah"))
;; (blah 23 #t blah (blah 23 #t blah))
;; '(blah 23 #t "blah" (blah 23 #t "blah"))
;;
;; proc
;; #<procedure:dump>
;; #<procedure:dump>
;; #<procedure:dump>
