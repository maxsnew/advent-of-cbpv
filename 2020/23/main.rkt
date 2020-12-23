#lang fiddle
(require fiddle/prelude
         fiddle/stdlib/IO
         "solution.rkt")

(! apply/cmd-line
   (~ (copat [("a" "input" #:bind) (! idiom^ (~! displayall 'answer:) (~! main-a input))]
             [("a" "sample" #:bind) (! idiom^ (~! displayall 'answer:) (~! main-a sample))]
             [("b" "input" #:bind) (! idiom^ (~! displayall 'answer:) (~! main-b input))]
             [("b" "sample" #:bind) (! idiom^ (~! displayall 'answer:) (~! main-b sample))]
             [() (! error "usage: racket main.rkt (a|b) (sample|input)")])))
