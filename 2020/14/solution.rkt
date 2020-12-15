#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Eff
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
     % n> (~! cl-map debug)
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

(def-thunk (! choose t1 t2)
  (! bindE (~! raiseE 'flip)
     (~ (copat [(#t) (! t1)]
               [(#f) (! t2)]))))

(def/copat (! modify-bit)
  [(#\X b) (! choose (~! retE 0) (~! retE 1))]
  [(#\0 b) (! retE b)]
  [(#\1 _) (! retE 1)])

(def/copat (! mod-and-cons)
  [((list mb vb) tl)
   (! mapE Cons (~! modify-bit mb vb) tl)])

(def-thunk (! mask-addr mask addr)
  (! CBN (~! cl-zipwith (~! colist<-string mask) (~! colist<-list addr))
     % n> (~! cl-map (~ (copat [((list #\X _)) (ret 'X)]
                               [((list #\0 b)) (ret b)]
                               [((list #\1 _)) (ret 1)])))
     % n> list<-colist
     % n$))

(def-thunk (! decode-addr mask loc)
  (! CBN (~! cl-zipwith (~! colist<-string mask) (~! colist<-list loc))
     % n> (~! cl-foldr^ mod-and-cons (~! retE '()))
     % n> (~ (λ (t) (! handle t List
                       (~ (copat [('flip resume)
                                  (! idiom^ append (~! resume #t) (~! resume #f))])))))
     % n$))

;; A Mem-Trie A is either
;; (list 'none)
;; (list 'val A)
;; (list 'amb (Mem-Trie A))
;; (list '01 (Mem-Trie A) (Mem-Trie A))
(define empty-trie (list 'none))
(def-thunk (! singleton bits x)
  (cond [(! empty? bits) (ret (list 'val x))]
        [else
         [bit <- (! first bits)]
         [bits <- (! rest bits)]
         [t <- (! singleton bits x)]
         (pat bit
              [0 (ret (list '01 t empty-trie))]
              [1 (ret (list '01 empty-trie t))]
              ['X (ret (list 'amb t))])]))

(def-thunk (! ins-mem-trie bits x t)
  ;; (! displayall 'ins bits x t)
  (cond [(! empty? bits) (ret (list 'val x))]
        [else
         [bit <- (! first bits)]
         [tag <- (! first t)]
         [bits <- (! rest bits)]
         (pat (list bit tag)
              [(list _ 'none) (! singleton (cons bit bits) x)]
              [(list _ 'val) (! error "should never happen since everything's the same length")]
              [(list 'X 'amb)
               [tl <- (! second t)]
               [tl-t <- (! ins-mem-trie bits x tl)]
               (ret (list 'amb tl-t))]
              [(list '0 'amb)
               [tl <- (! second t)]
               [l <- (! ins-mem-trie bits x tl)]
               (ret (list '01 l tl))]
              [(list '1 'amb)
               [tl <- (! second t)]
               [r <- (! ins-mem-trie bits x tl)]
               (ret (list '01 tl r))]
              [(list 'X '01)
               [l <- (! second t)]
               [r <- (! third t)]
               [l <- (! ins-mem-trie bits x l)]
               [r <- (! ins-mem-trie bits x r)]
               (ret (list '01 l r))]
              [(list 0 '01)
               [l <- (! second t)]
               [r <- (! third t)]
               [l <- (! ins-mem-trie bits x l)]
               (ret (list '01 l r))]
              [(list 1 '01)
               [l <- (! second t)]
               [r <- (! third t)]
               [r <- (! ins-mem-trie bits x r)]
               (ret (list '01 l r))])]))

(def/copat (! add-locs-loop)
  [((list 'none)) (ret 0)]
  [((list 'val x)) (ret x)]
  [((list 'amb t)) (! <<v * 2 'o add-locs-loop t)]
  [((list '01 l r)) (! idiom^ + (~! add-locs-loop l) (~! add-locs-loop r))])

(def-thunk (! add-all-locs trie)
  (! add-locs-loop trie))

(def-thunk (! b-step (list cur-mask mem))
  (copat [((list new-mask)) (ret (list new-mask mem))]
         [((list loc val))
          [b-loc <- (! to-bits loc)]
          [decoded-locs <- (! decode-addr cur-mask b-loc)]
          [mem-locs <- (! map from-bits decoded-locs)]
          [mem <- (! foldl mem-locs (~ (λ (mem loc) (! mem 'set loc val))) mem)]
          (ret (list cur-mask mem))]))

(def-thunk (! b-step^ (list cur-mask mem))
  (copat [((list new-mask)) (ret (list new-mask mem))]
         [((list loc val))
          [b-loc <- (! to-bits loc)]
          [loc <- (! mask-addr cur-mask b-loc)]
          ;; (! displayall mem)
          [mem <- (! ins-mem-trie loc val mem)]
          (ret (list cur-mask mem))]))

#;
(def-thunk (! main-b f)
  (patc (! cl-foldl (~! parse f) b-step (list id-mask empty-table))
        [(list final-mask mem)
         (! CBV (~! mem 'to-list)
            % v> (~! map cdr)
            % v> (~! foldl^ + 0)
            % v$)]))

(def-thunk (! main-b f)
  (patc (! cl-foldl (~! parse f) b-step^ (list id-mask empty-trie))
        [(list final-mask mem)
         (! add-locs-loop mem)]))

(def-thunk (! main-c f)
  (! CBN (~! parse f)
     % n> (~! cl-filter (~! <<v = 1 'o length))
     % n> (~! cl-map first)
     % n> (~! cl-map string->list)
     % n> (~! cl-map (~! filter (~! equal? #\X)))
     % n> (~! cl-map length)
     % n> list<-colist
     % n$
     ))

(provide main-a main-b main-c)