#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/IO)
(require fiddle/stdlib/CoList)
(require fiddle/stdlib/Eff)
(require "../Parse.rkt")
(require "../Effcode.rkt")

(provide main-a main-b)

(def-thunk (! io t inp)
  (! handle t
     error
     (~ (copat [('input resume) (! resume inp)]
               [((list 'output o) resume) (ret o)]))))

;; (def/copat (! print-outputs-driver inp)
;;   [((= 'input) k) (! k inp (~ (! print-outputs-driver inp)))]
;;   [((= 'output) o k) (! displayall o) (! k (~ (! print-outputs-driver inp)))]
;;   [((= 'halt)) (ret 'done)])

(def-thunk (! main i)
  [syn <- (! parse-intcode-program)]
  (! <<v displayall 'o io (~! effcode syn) i))

(def-thunk (! main-a) (! main 1))

(def-thunk (! main-b) (! main 2))
