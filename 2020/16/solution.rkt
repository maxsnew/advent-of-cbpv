#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Eff
         fiddle/stdlib/Table
         fiddle/stdlib/IO
         "../../regex.rkt"
         )

(define! lower-case/lex
  (! idiom^ (~! List list->string) (~! idiom^ compile-regex (~! idiom^ star/e (~! char/e "abcdefghijklmnopqrstuvwxyz")))))

(define! space/lex
  (! char/lex ",:- " (~! abort 'space)))
(define! newline/lex
  (! char/lex "\n" (~! abort 'newline)))

(define sig (list lower-case/lex unsigned-number/lex space/lex newline/lex))

(def-thunk (! parse f)
   (! CBN (~! <<v fold-lex-string sig 'o (~! read-all-string f))
     % n> (~! cl-filter (~! <<v not 'o equal? 'space))
     % n> (~! sep-by 'newline)
     % n> (~! sep-by '())
     % n$))

(def-thunk (! main-a f)
  (! CBN  (~! parse f)
     % n> (~! cl-map debug)
     % n> cl-length
     % n$)
  )

(def-thunk (! main-b f)
  (! error 'nyi)
  )

(provide main-a main-b)
