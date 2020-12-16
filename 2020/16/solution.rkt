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

(def-thunk (! between lo hi x)
  (! and (~! <= lo x) (~! <= x hi)))

(def-thunk (! p<-2ranges lo1 hi1 _ lo2 hi2)
  (ret (~ (λ (x)
            (! or (~! between lo1 hi1 x)
                  (~! between lo2 hi2 x))))))

(def/copat (! p<-rule)
  [(name x)
   (cond [(! number? x) (! p<-2ranges x)]
         [else          (! p<-2ranges)])])

(def-thunk (! violates-all? ps x)
  (! CBN (~! colist<-list ps)
     % n> (~! cl-map (~! <<v not 'o swap $ x))
     % n> all?
     % n$))

(def-thunk (! violations ps nums)
  (patc (! CBN  (~! colist<-list nums)
           % n> (~! cl-filter (~! violates-all? ps))
           % n> (~! idiom^ view)
           % n$)
        [(cons n _) (ret (list n))]
        [x (ret x)]))

(def-thunk (! main-a f)
  (patc (! list<-colist (~! parse f))
    [(list rules (cons _ my-ticket) (cons _ nearby-tix))
     (! displayall 'parsed)
    [ps <- (! map (~! apply p<-rule) rules)]
    (! CBN  (~! colist<-list nearby-tix)
       % n> (~! cl-map (~! violations ps))
       % n> (~! cl-bind^ colist<-list)
       % n> (~! cl-map debug)
       % n> (~! cl-foldl^ + 0)
       % n$)]))

(def-thunk (! main-b f)
  (! error 'nyi)
  )

(provide main-a main-b)
