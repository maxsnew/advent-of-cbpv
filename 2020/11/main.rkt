#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/IO
         "solution.rkt")



(! apply/cmd-line (~ (copat [(f #:bind) (! idiom^ (~! displayall 'filled-seats:) (~! main-a f))]
                            [() (! error "usage: racket main.rkt INPUTFILE")])))
