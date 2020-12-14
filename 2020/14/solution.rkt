#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Table
         fiddle/stdlib/IO
         "../../regex.rkt"
         )

(define! mask-sym/lex (! exact-string/lex "mask" 'mask))
(define! mem-sym/lex (! exact-string/lex "mem" 'mem))
(define! punct/lex (! char/lex "[]= \n" (~! abort 'space)))
(define! mask/lex
  (! <<v (~! List list->string) 'o compile-regex 'o star/e 'o char/e "X10"))

(define sig (list unsigned-number/lex mask-sym/lex mem-sym/lex punct/lex mask/lex))

(def-thunk (! parse f)
   (! CBN (~! <<v fold-lex-string sig 'o (~! read-all-string f))
     % n> (~! cl-filter (~! <<v not 'o equal? 'space))
     % n> (~! sep-when (~ (λ (c) (! or (~! equal? 'mem c) (~! equal? 'mask c)))))
     % n> tl
     ;; % n> (~! cl-map debug)
     % n$))

(def-thunk (! from-bits xs)
  [*2+ = (~ (λ (old b) (! <<v + b 'o * 2 old)))]
  (! foldl xs *2+ 0))

;; String_32 -> 
(def-thunk (! mask-val mask val)
  (! CBN (~! cl-zipwith (~! colist<-string mask) (~! colist<-list val))
     % n> (~! cl-map (~ (copat [((list #\X b)) (ret b)]
                               [((list #\0 _)) (ret 0)]
                               [((list #\1 _)) (ret 1)])))
     % n> list<-colist
     % n$))

(def/copat (! fill-zeros acc)
  [(0) (ret acc)]
  [(n)
   [n <- (! - n 1)]
   (! fill-zeros (cons 0 acc) n)])

(def-thunk (! to-bits-loop x acc num-bits)
  (cond [(! = 0 x) (! fill-zeros acc num-bits)]
        [else
         [b <- (! modulo x 2)]
         [q <- (! quotient x 2)]
         [num-bits <- (! - num-bits 1)]
         (! to-bits-loop q (cons b acc) num-bits)]))

;; -> F (String_36 (0|1))
(def-thunk (! to-bits v)
  (! to-bits-loop v '() 36))

(def-thunk (! a-step (list cur-mask mem))
  (copat [((list new-mask)) (ret (list new-mask mem))]
         [((list loc val))
          [b-val <- (! to-bits val)]
          [masked-val <- (! mask-val cur-mask b-val)]
          [mem-val <- (! from-bits masked-val)]
          [mem <- (! mem 'set loc mem-val)]
          (ret (list cur-mask mem))]))

(define id-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")

(def-thunk (! main-a f)
  (patc (! cl-foldl (~! parse f) a-step (list id-mask empty-table))
        [(list final-mask mem)
         (! CBV (~! mem 'to-list)
            % v> (~! map cdr)
            % v> (~! foldl^ + 0)
            % v$)]))

(def-thunk (! main-b f)
  (ret 'nyi))

(provide main-a main-b)
