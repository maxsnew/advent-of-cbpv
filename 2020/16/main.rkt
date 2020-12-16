#lang fiddle
(require fiddle/prelude
         fiddle/stdlib/IO
         "solution.rkt")

(! apply/cmd-line
   (~ (copat [("a" f #:bind) (! idiom^ (~! displayall 'answer:) (~! main-a f))]
             [("b" f #:bind) (! idiom^ (~! displayall 'answer:) (~! main-b f))]
             [() (! error "usage: racket main.rkt (a|b) INPUTFILE")])))
