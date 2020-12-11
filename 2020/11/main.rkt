#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/IO
         "solution.rkt")



(! apply/cmd-line
   (~ (copat [("a" f #:bind) (! idiom^ (~! displayall 'filled-seats:) (~! main-a f))]
             [("b" f #:bind) (! idiom^ (~! displayall 'filled-seats:) (~! main-b f))]
             [() (! error "usage: racket main.rkt (a|b) INPUTFILE")])))
