#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/IO
         "../../regex.rkt"
         )

(define! comma/lex (! exact-string/lex "," 'comma))
(define! newline/lex (! exact-string/lex "\n" 'newline))
(define! x/lex (! exact-string/lex "x" 'x))

(define sig (list unsigned-number/lex comma/lex x/lex newline/lex))

(def-thunk (! parse inp)
  (! CBN (~! <<v fold-lex-string sig 'o list->string 'o list<-colist inp)
     % n> (~! cl-filter (~! <<v not 'o equal? 'comma))
     % n> (~! sep-by 'newline)
     % n> list<-colist
     % n$))

(def-thunk (! time-till-next earliest time)
  (! idiom^ (~! - time) (~! modulo earliest time)))

(def-thunk (! part-a earliest times)
  [id <- (! CBN (~! colist<-list times)
       % n> (~! minimum-by1 (~! time-till-next earliest))
       % n$)]
  (! <<v * id 'o time-till-next earliest id ))

(def-thunk (! main-a f)
  (patc (! parse (~! read-all-chars f))
        [(list (list earliest) ids)
         [ids <- (! filter number? ids)]
         (! part-a earliest ids)]))

(def-thunk (! main-b f)
  (! error 'nyi))

(provide main-a main-b)
