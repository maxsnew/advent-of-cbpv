#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList)

(provide initialize-memory
         memory-get
         memory-set!
         ;; colist<-memory
         )

;; A Memory is implemented as a Box (Vec Int)

(define-thunk (! initialize-memory l)
  (do [b <- (! <<v box 'o list->vector l '$)]
      (ret b)))

(def-thunk (! maybe-grow b ix)
  [v <- (! unbox b)]
  [len <- (! vector-length v)]
  (cond [(! < ix len) (ret '())]
        [else
         [ix*2 <- (! * 2 ix)]
         (! displayall 'allocating-to ix*2)
         [v^ <- (! make-vector ix*2 0)]
         (! <<n cl-foreach (~ (λ (ix)
                                (do [x <- (! vector-ref v ix)]
                                    (! vector-set! v^ ix x)))) 'o
            range 0 len '$)
         (! set-box! b v^)]))

(define-thunk (! memory-get mem ix)
  (do (! maybe-grow mem ix)
      [v <- (! unbox mem)]
    (! vector-ref v ix)))

(define-thunk (! memory-set! mem ix val)
  (do (! maybe-grow mem ix)
      [v <- (! unbox mem)]
    [old <- (! vector-ref v ix)]
    (! vector-set! v ix val)
    (ret old)))


;; (define-thunk ! )
