#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/IO
         "../../regex.rkt")

(define! sig (! idiom^ List
                (~! exact-string/lex "N" 'N)
                (~! exact-string/lex "S" 'S)
                (~! exact-string/lex "E" 'E)
                (~! exact-string/lex "W" 'W)
                (~! exact-string/lex "L" 'L)
                (~! exact-string/lex "R" 'R)
                (~! exact-string/lex "F" 'F)
                (~! exact-string/lex "\n" 'space)
                (~ (ret unsigned-number/lex))))

(def-thunk (! parse c)
  (! CBN (~! <<v fold-lex-string sig 'o list->string 'o list<-colist c)
     % n> (~! cl-filter (~! <<v not 'o equal? 'space ))
     % n> (~! chunks 2)
     ;; % n> (~! cl-map debug)
     % n$))
