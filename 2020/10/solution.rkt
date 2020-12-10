#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/IO
         "../../regex.rkt")


;; Parsing
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

(def-thunk (! insert-diff (list 1diffs 2diffs 3diffs) (list lo hi))
  (patc (! - hi lo)
    [1 [1diffs <- (! + 1 1diffs)]
     (ret (list 1diffs 2diffs 3diffs))]
    [2 [2diffs <- (! + 1 2diffs)]
     (ret (list 1diffs 2diffs 3diffs))]
    [3 [3diffs <- (! + 1 3diffs)]
     (ret (list 1diffs 2diffs 3diffs))]))

(def-thunk (! main-a f)
  [inp <- (! <<v swap sort < 'o list<-colist (~! parse-file f))]
  (patc (! CBN (~! cl-cons 0 (~! colist<-list inp))
           % n> windows2
           % n> (~! cl-foldl^ insert-diff (list 0 0 1))
           % n$)
        [(list 1diffs _ 3diffs)
         (! * 1diffs 3diffs)]))

