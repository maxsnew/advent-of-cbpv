#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/IO
         "../../regex.rkt"
         )

(define! mask-sym/lex (! exact-string/lex "mask" 'mask))
(define! mem-sym/lex (! exact-string/lex "mem" 'mem))
(define! punct/lex (! char/lex "[]= \n" (~! abort 'space)))
(define! mask/lex
  (! <<v (~! List Ret) 'o compile-regex 'o star/e 'o char/e "X10"))

(define sig (list unsigned-number/lex mask-sym/lex mem-sym/lex punct/lex mask/lex))

(def-thunk (! main-a f)
  (! CBN (~! <<v fold-lex-string sig 'o (~! read-all-string f))
     % n> (~! cl-filter (~! <<v not 'o equal? 'space))
     % n> (~! sep-by 'mem)
     % n> list<-colist
     % n$))

(def-thunk (! main-b f)
  (ret 'nyi))

(provide main-a main-b)
