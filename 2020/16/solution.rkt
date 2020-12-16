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

(def-thunk (! valid-ticket? ps t)
  (patc (! violations ps t)
        ['() (ret #t)]
        [_   (ret #f)]))

(def/copat (! valid-rule? num)
  [((list name p)) (! p num)]
  [() (! error 'wtf)])

(def-thunk (! andb b1 b2)
  (if b1
      (ret b2)
      (ret #f)))

(def-thunk (! remove-rule! v except-ix rule)
  [l <- (! vector-length v)]
  (! CBN (~! range 0 l)
     % n> (~! cl-filter (~! <<v not 'o = except-ix))
     % n> (~! cl-foreach (~ (λ (ix)
                              (do [rules <- (! vector-ref v ix)]
                                  [rules <- (! filter (~! <<v not 'o equal? rule) rules)]
                                (! vector-set! v ix rules)))))
     % n$))

(def-thunk (! singleton? xs)
  [tl <- (! cdr xs)]
  (! null? tl))

#;
(def-thunk (! eliminate-bad-rules rules-vec fields k)
  (! displayall 'next-ticket: fields)
  [b <- (! CBN (~! cl-zipwith (~! colist<-list fields) (~! range 0))
       % n> (~! cl-map (~ (copat [((list num ix))
                                  [cand-rules <- (! vector-ref rules-vec ix)]
                                  (cond [(! singleton? cand-rules)
                                         ;; (! displayall 'already-determined ix cand-rules)
                                         (ret #t)]
                                        [else
                                         [valid-rules <- (! filter (~! valid-rule? num) cand-rules)]
                                         (! vector-set! rules-vec ix valid-rules)
                                         (pat valid-rules
                                              [(list rule)
                                               [rule-name <- (! first rule)]
                                               (! displayall 'determined-a-new-rule ix rule-name)
                                               (! remove-rule! rules-vec ix rule)
                                               (ret #t)]
                                              [_           (ret #f)])])])))
       % n> (~! cl-foldl^ andb #t)
       % n$)]
  (if b
      (ret '())
      (! k)))

;; U(Table K (Listof V)) -> Listof K -> V -> U(Table K (Listof V))
(def-thunk (! remove-val tbl ks v)
  (! foldl ks
     (~ (λ (tbl k)
          (do [vs <- (! tbl 'get k #f)]
              [vs <- (! filter (~! <<v not 'o equal? v) vs)]
            (! tbl 'set k vs))))
     tbl))
#;
(def-thunk (! eliminate-bad-rules fields k fields->rules rules->fields)
  (! displayall 'next-ticket: fields)
  [b <- (! CBN (~! cl-zipwith (~! colist<-list fields) (~! range 0))
       % n> (~! cl-map (~ (copat [((list num ix))
                                  [cand-rules <- (! fields->rules 'get ix #f)]
                                  (cond [(! singleton? cand-rules)
                                         ;; (! displayall 'already-determined ix cand-rules)
                                         (ret #t)]
                                        [else
                                         (patc (! partition (~! valid-rule? num) cand-rules)
                                               [(list valid-rules invalid-rules)
                                                [fields->rules <- (! fields->rules 'set ix valid-rules)]
                                                [rules->fields <- (! remove-fields rules->fields invalid-rules ix)]
                                                []])
                                         (! vector-set! rules-vec ix valid-rules)
                                         (pat valid-rules
                                              [(list rule)
                                               [rule-name <- (! first rule)]
                                               (! displayall 'determined-a-new-rule ix rule-name)
                                               (! remove-rule! rules-vec ix rule)
                                               (ret #t)]
                                              [_           (ret #f)])])])))
       % n> (~! cl-foldl^ andb #t)
       % n$)]
  )

#;
(def-thunk (! determine-fields named-rules valid-tix)
  [len <- (! length named-rules)]
  [fields->rules <- (! <<v table<-list 'o list<-colist (~! cl-zipwith (~! colist<-list named-rules) (~! range 0)))] ;; (Map Ix (Listof NamedRule))
  ;; (! make-vector len named-rules)
  [rules->fields <- (! <<v table<-list 'o list<-colist (~! cl-zipwith (~! colist<-list named-rules) (~! range 0)))]
  (patc (! cl-foldr ;; Loop invariant B = F '()
           valid-tix eliminate-bad-rules List fields->rules rules->fields)
        [(list fields->rules rules->fields)
           [remaining <- (! fields->rules 'to-list)]
           (! map (~! displayall 'possible-rules:) remaining)
           (! <<v map first 'o map first remaining)]))

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
     (! displayall 'my-ticket: my-ticket)
     [possibilities <- (! <<v map car 'o good-rule*ixs 'to-list)]
     (! cl-foreach displayall (~! colist<-list possibilities))
     (! displayall 'need: l)
     (! length possibilities)
     #;
     (! CBN (~! cl-zipwith (~! colist<-list rules) (~! colist<-list my-ticket))
        % n> (~! cl-filter (~! <<v equal? "departure" 'o first 'o first))
        % n> (~! cl-map debug)
        % n> (~! cl-map second)
        % n> (~! cl-foldl^ * 1)
        % n$)]))

(provide main-a main-b)
