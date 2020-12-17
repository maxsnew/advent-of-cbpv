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

(def/copat (! named-rule)
  [(name0 x (rest args))
   (cond [(! number? x) (! idiom^ (~! List (list name0)) (~! apply (~! p<-2ranges x) args))]
         [else          (! idiom^ (~! List (list name0 x)) (~! apply p<-2ranges args))])])

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

;; Part b Sketch:
;; make a table : FieldIx x Rule -> Bool
;; initially all #t
;
;; Iterate through all fields in each ticket, paired with each rule
;; - if the field is valid for the rule, ok
;; - otherwise set the table to #f
(def-thunk (! all-pairs tix rules)
  [rules~ = (~! colist<-list rules)]
  (! CBN (~! colist<-list tix)
     % n> (~! cl-bind^ (~ (λ (ticket)
                            (! cartesian-product
                               rules~
                               (~! cl-zipwith (~! colist<-list ticket) (~! range 0))))))
     % n> (~! cl-map (~ (copat [((list rule (list field-val ix)))  (ret (list field-val rule ix))])))
     ;; % n> (~! cl-map debug)
     % n$))

;; valid? : Number -> Rule -> F Bool
(def-thunk (! valid? n named-rule)
  [p <- (! second named-rule)]
  (! p n))

;
;; Once this is over, Split the relation table into two function tables:
;; FieldIx -> Set Rule
;; Rule    -> Set FieldIx
;; and initialize a solution (Listof (List FieldIx Rule))
;
;; Go through the two tables and find a (FieldIx Rule) pair that is unique on one side.
;; Add that pair to the solution list
;; And remove the FieldIx and Rule from the domain and codomain of both tables
;
;; Repeat until the tables are empty, in which case the solution set will be full!

(def-thunk (! good-tix rules nearby-tix)
  [ps <- (! map (~! apply p<-rule) rules)]
  (! CBN  (~! colist<-list nearby-tix)
     % n> (~! cl-filter (~ (λ (ticket)
                             (patc (! violations ps ticket)
                                   ['() (ret #t)]
                                   [_ (ret #f)]))))
     % n> list<-colist
     % n$))

(def-thunk (! push-tbl t k v)
  (! update~ t k (~! empty-table 'set v #t) (~! swap apply (list 'set v #t))))

(def-thunk (! pop-all k->vs v)
  [ks <- (! <<v map car 'o k->vs 'to-list)]
  (! cl-foldl (~! colist<-list ks)
     (~ (λ (k->vs k)
          (do [vs <- (! oo k->vs 'get k #f '@ 'remove v)]
              (! k->vs 'set k vs))))
     k->vs))

(def-thunk (! find-solved k->vs)
  (cond [(! k->vs 'empty?) (ret 'done)]
        [else
         (! cl-foldr (~! <<v colist<-list 'o k->vs 'to-list)
            (~ (copat [((cons key tbl) kont)
                       (patc (! tbl 'to-list)
                             [(list (cons val _)) (ret (list key val))]
                             [_ (! kont)])]))
            (~! Ret 'none))]))

(def-thunk (! solve-constraints rules->ixs ixs->rules solved)
  (patc (! find-solved rules->ixs)
        ['done (ret solved)]
        ['none
         (! displayall 'plz-dont-loop)
         (! solve-constraints ixs->rules rules->ixs solved)]
        [(list rule ix)
         (! displayall 'solved1: rule ix)

         [rules->ixs <- (! rules->ixs 'remove rule)]
         [rules->ixs <- (! pop-all rules->ixs ix)]

         [ixs->rules <- (! ixs->rules 'remove ix)]
         [ixs->rules <- (! pop-all ixs->rules rule)]

         [solved = (cons (list rule ix) solved)]
         (! solve-constraints rules->ixs ixs->rules solved)]))

(def-thunk (! curry-relation rel)
  (! displayall 'currying)
  (! cl-foldr (~! <<v colist<-list 'o rel 'to-list)
     (~ (copat [((cons (list l r) _) k l->rs r->ls)
                [l->rs <- (! push-tbl l->rs l r)]
                [r->ls <- (! push-tbl r->ls r l)]
                (! k l->rs r->ls)]))
     List
     empty-table
     empty-table))

(def-thunk (! main-b f)
  (patc (! list<-colist (~! parse f))
    [(list rules (list _ my-ticket) (cons _ nearby-tix))
     (! displayall 'parsed)
     [nearby-tix <- (! good-tix rules nearby-tix)]
     (! displayall 'good-tix-only)
     [named-rules <- (! map (~! apply named-rule) rules)]
     [l <- (! length named-rules)]
     [good-rule*ixs <- (! <<v table-set<-list 'o list<-colist (~! cartesian-product (~! colist<-list named-rules) (~! range 0 l)))]
     [good-rule*ixs <- (! CBN (~! all-pairs nearby-tix named-rules)
          % n> (~! cl-foldl^
                   (~ (copat [(good-rule*ixs (list num (list rule p) ix))
                              (cond [(! p num) (ret good-rule*ixs)]
                                    [else
                                     ;; (! displayall 'invalid-pair: rule ix num)
                                     (! good-rule*ixs 'remove (list (list rule p) ix))])]))
                   good-rule*ixs)
          % n$)]
     (patc (! curry-relation good-rule*ixs)
           [(list rules->ixs ixs->rules)
            (! displayall 'curry-time-over)
            (! <<v displayall 'rules->ixs 'o rules->ixs 'to-list)
            (! <<v displayall 'ixs->rules 'o ixs->rules 'to-list)
            [rules<->ixs <- (! solve-constraints rules->ixs ixs->rules '())]
            [my-ticket-v <- (! list->vector my-ticket)]
            (! CBV (~! filter (~! <<v equal? "departure" 'o first 'o first 'o first) rules<->ixs)
               % v> (~! map debug)
               % v> (~! map second)
               % v> (~! map (~! vector-ref my-ticket-v))
               % v> (~! foldl^ * 1)
               % v$)])]))

(provide main-a main-b)
