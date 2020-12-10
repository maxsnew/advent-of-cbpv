#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/IO
         "../../regex.rkt")

(define! space/lex
  (! exact-string/lex "\n" 'space))
(define sig (list space/lex number/lex))

(def-thunk (! parse-colist c)
  (! CBN c
     % n> (~! fold-lex sig)
     % n> (~! cl-filter (~! <<v not 'o equal? 'space))
     % n$))

(def-thunk (! parse-file f)
  (! parse-colist (~! read-all-chars f)))
