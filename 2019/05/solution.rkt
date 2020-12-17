#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/IO)
(require fiddle/stdlib/CoList)
(require "../Parse.rkt")
(require fiddle/stdlib/FlexVec)
(require "../Effcode.rkt")
(require fiddle/stdlib/Eff)

(provide main-a main-b)

(def-thunk (! run-with-input x)
  [syn <- (! parse-intcode-program)]
  [ans <- (! handle (~! effcode syn)
       (~! error 'intcode-never-returns!?!??)
       (~ (copat
           [(           'halt resume o) (ret o)]
           [(          'input resume)
            (! displayall 'supplying x)
            (! resume x)]
           [((list 'output o) resume _)
            (! displayall 'output: o)
            (! resume '() o)]))
       #f)]
  (! displayall 'result: ans))

(def-thunk (! main-a) (! run-with-input 1))

(def-thunk (! main-b) (! run-with-input 5))
