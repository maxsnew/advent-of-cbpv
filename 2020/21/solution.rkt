#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Eff
         fiddle/stdlib/Table
         fiddle/stdlib/IO
         "../../regex.rkt"
         )


(define! letter/e (! char/e "abcdefghijklmnopqrstuvwxyz"))
(define! word/e  (! <<v compile-regex 'o +/e letter/e))
(define word/lex (list list->string word/e))
(define! space/lex (! char/lex "(), " (~! abort 'space)))
(define! newline/lex (! char/lex "\n" (~! abort 'newline)))
(define sig (list word/lex space/lex newline/lex))

;; Path -> CoList (List (Listof Ing) (Listof Allergen))
(def-thunk (! parse f)
   (! CBN (~! <<v fold-lex-string sig 'o (~! read-all-string f))
     % n> (~! cl-filter (~! <<v not 'o equal? 'space))
     % n> (~! sep-by 'newline)
     % n> (~! cl-map (~ (λ (x)
                          (! <<n list<-colist 'o sep-by "contains" 'o colist<-list x))))
     % n$))

;; V similar to day 16 (matching up fields to items on the passport)

;; The map from Allergens to Ingredients is total, injective
;; The map from Ingredients to Allergens is partial, but surjective and injective

;; Each line lists all of the ingredients, but maybe only some of the
;; allergens.

;; Step 1: Build a Table of facts TableSet (List (Listof Ingredients) Allergen)

;; Step 2: Curry the relation to get a Table Allergen (Listof (Listof Ingredients))

;; Step 3: Intersect the vals to get a Table Allergen (Setof Ingredients)

;; U(Table K (TableSet V)) -> V -> U(Table K (TableSet V))
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

(def-thunk (! solve-constraints allergens->ings solved)
  (patc (! find-solved allergens->ings)
        ['done (ret solved)]
        [(list allergen ing)
         (! displayall 'solved1: allergen ing)

         [allergens->ings <- (! allergens->ings 'remove allergen)]
         [allergens->ings <- (! pop-all allergens->ings ing)]

         [solved = (cons (list allergen ing) solved)]
         (! solve-constraints allergens->ings solved)]))


(def-thunk (! main-a f)
  [warnings <- (! list<-colist (~! parse f))]
  [assocs <- (! foldl warnings
       (~ (copat [(tbl (list ings allergens))
                  (! foldl allergens
                     (~ (λ (tbl allergen)
                          (! tbl 'set (list ings allergen) #t)))
                     tbl)]))
       empty-table)]
  (! displayall 'parsed)
  (patc (! split-adjacency-tbl assocs)
   [(list _ allergens->ing-lists)
    [allergens->ings <- (! map-vals
                           (~ (λ (ing-lists)
                                (! <<v
                                   ;; map car 'o
                                   ;; swap $ 'to-list 'o
                                   foldl1^ table-set-intersect 'o
                                   map table-set<-list ing-lists)))
                           allergens->ing-lists)]
    [allergen*ings <- (! solve-constraints allergens->ings '())]
    (! displayall 'allergens: allergen*ings)
    [allergenic-ings <- (! map second allergen*ings)]
    [allergenic-ings~ = (~! colist<-list allergenic-ings)]
    ;; Listof (List (Listof Ing) Allergen)
    (! CBN (~! colist<-list warnings)
       % n> (~! cl-map first)
       % n> (~! cl-bind^ colist<-list)
       % n> (~! cl-filter (~! <<v not 'o swap member? allergenic-ings~))
       ;; % n> (~! cl-map debug)
       % n> cl-length
       % n$)
    ;; (! CBN  (~! colist<-list assoc-list)
    ;;    % n> )
    ]))

(def-thunk (! main-b f)
  [assocs <- (! cl-foldl (~! parse f)
       (~ (copat [(tbl (list ings allergens))
                  (! foldl allergens
                     (~ (λ (tbl allergen)
                          (! tbl 'set (list ings allergen) #t)))
                     tbl)]))
       empty-table)]
  (! displayall 'parsed)
  (patc (! split-adjacency-tbl assocs)
   [(list _ allergens->ing-lists)
    [allergens->ings <- (! map-vals
                           (~ (λ (ing-lists)
                                (! <<v
                                   ;; map car 'o
                                   ;; swap $ 'to-list 'o
                                   foldl1^ table-set-intersect 'o
                                   map table-set<-list ing-lists)))
                           allergens->ing-lists)]
    [allergen*ings <- (! solve-constraints allergens->ings '())]
    (! CBV (~! sort allergen*ings (~ (λ (x y)
                                       (do [x <- (! first x)]
                                           [y <- (! first y)]
                                         (! string<=? x y)))))
       % v> (~! map (~! <<v second 'o debug))
       % v> (~! foldl1^ (~ (λ (s ing) (! string-append s "," ing))))
       % v$)
    ;; (! CBN  (~! colist<-list assoc-list)
    ;;    % n> )
    ]))

(provide main-a main-b)
