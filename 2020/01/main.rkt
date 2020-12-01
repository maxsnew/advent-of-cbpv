#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/IO
         "solution.rkt")

(def-thunk (! run f x) (! <<v displayall 'o f x))
(def/copat (! main)
  [("1" "sample") (! run a-soln sample-input~)]
  [("1" "input") (! run a-soln a-input~)]
  [("1fast" "sample") (! run a-soln^ sample-input)]
  [("1fast" "input") (! run a-soln^ a-input)]
  [("2" "sample") (! run faster-faster-b sample-input~)]
  [("2" "input") (! run faster-faster-b a-input~)]
  [("2fast" "sample") (! run b-soln^ sample-input)]
  [("2fast" "input") (! run b-soln^ a-input)])

(! apply/cmd-line main)
