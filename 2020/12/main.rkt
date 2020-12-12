#lang fiddle
(require fiddle/prelude
         fiddle/stdlib/IO
         "solution.rkt")



(! apply/cmd-line
   (~ (copat [("a" f #:bind) (! idiom^ (~! displayall 'manhattan-distance-from-start:) (~! main-a f))]
             [("b" f #:bind) (! idiom^ (~! displayall 'manhattan-distance-from-start:) (~! main-b f))]
             [() (! error "usage: racket main.rkt (a|b) INPUTFILE")])))
