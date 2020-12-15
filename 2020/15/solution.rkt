#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Eff
         fiddle/stdlib/Table
         fiddle/stdlib/IO
         "../../regex.rkt"
         )

(define! comma/lex (! char/lex ",\n" (~! abort 'space)))
(define sig (list unsigned-number/lex comma/lex))

(def-thunk (! parse f)
   (! CBN (~! <<v fold-lex-string sig 'o (~! read-all-string f))
     % n> (~! cl-filter (~! <<v not 'o equal? 'space))
     % n$))

;; A Search State is a (Table Number Number) that associates each
;; number to its largest index
;
;;
(def-thunk (! step-a (list biggest-ix-tbl last-number last-ix))
  [cur-ix <- (! + 1 last-ix)]
  (cond [(! hash-has-key? biggest-ix-tbl last-number)
         [prev-ix <- (! hash-ref biggest-ix-tbl last-number)]
         [diff <- (! - last-ix prev-ix)]
         [biggest-ix-tbl <- (! hash-set biggest-ix-tbl last-number last-ix)]
         (ret (cons diff (list biggest-ix-tbl diff cur-ix)))]
        [else ;; new number
         [biggest-ix-tbl <- (! hash-set biggest-ix-tbl last-number last-ix)]
         (ret (cons 0 (list biggest-ix-tbl 0 cur-ix)))]))

(def-thunk (! loop stop? biggest-ix-tbl last-number last-ix )
  (cond [(! = stop? last-ix) (ret last-number)]
        [else
         [cur-ix <- (! + 1 last-ix)]
         [cur-num <-
                  (do [prev-ix <- (! vector-ref biggest-ix-tbl last-number)]
                      (if prev-ix
                          (! - last-ix prev-ix)
                          (ret 0)))]
         (! vector-set! biggest-ix-tbl last-number last-ix)
         (! loop stop? biggest-ix-tbl cur-num cur-ix)]))

;; (! oo empty-table 'set 0 1 '@ 'set 3 2)
;; (list init-table 6 3)
(def-thunk (! initialize start-nums capacity)
  [len <- (! length start-nums)]
  [tbl <- (! make-vector capacity #f)]
  (patc (! reverse start-nums)
        [(cons last rest)
         [all-but-last <- (! reverse rest)]
         (! cl-foreach (~ (copat [((list x ix)) (! vector-set! tbl x ix)]))
            (~! cl-zipwith (~! colist<-list all-but-last) (~! range 1)))
         (ret (list tbl last len))]))

(def-thunk (! soln n f)
  [inp <- (! list<-colist (~! parse f))]
  [init-state <- (! initialize inp n)]
  (! displayall 'input: inp)
  (! apply (~! loop n)
     init-state))

(def-thunk (! main-a) (! soln 2020))

(def-thunk (! main-b)
  (! soln 300000)
  ;; (! soln 30000000)
  )

(provide main-a main-b)
