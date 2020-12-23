#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Eff
         fiddle/stdlib/Table
         fiddle/stdlib/IO

         "../../regex.rkt"
         "lib.rkt"
         )
(define! newline/lex (! char/lex "\n" (~! abort 'newline)))
(define! sigil/lex (! char/lex "#." list->string))
(define sig (list sigil/lex newline/lex))

(def-thunk (! string<-char c) (! list->string (list c)))

(def-thunk (! parse f)
   (! CBN (~! <<v fold-lex-string sig 'o (~! read-all-string f))
     % n> (~! sep-by 'newline)
     % n> (~! cl-filter (~! <<v not 'o null?))
     % n$))

(def-thunk (! pts<-field rows~)
  [ls <- (! list<-colist
      (~! cl-map (~ (copat [((list x xs))
                            (! list<-colist
                               (~! cl-map (~ (copat [((list x (list y c)))
                                                     (! Cons (list x y) c)])) (~! cl-zipwith (~! forever x) xs)))]))
          (~! cl-zipwith
              (~! range 0)
              (~! cl-map (~ (λ (row)
                              (ret (~! cl-zipwith (~! range 0) (~! colist<-list row)))))
                  rows~))))]
  (! apply append ls))

(def-thunk (! tbl<-field rows~)
  (! <<v table<-list 'o
     filter (~! <<v equal? "#" 'o cdr) 'o
     pts<-field rows~))

(define! sea-monster-lines
  (! CBN (~! colist<-string "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   ")
     % n> (~! sep-by #\newline)
     % n> (~! cl-map (~! map string<-char))
     % n> list<-colist
     % n$))

(define! all-sea-monsters
  (! list<-colist (~! cl-map (~! swap orient-rows sea-monster-lines) (~! colist<-list all-orientations))))

(define! all-sea-monster-pts
  (! <<v
     reverse 'o
     map (~! map car) 'o
     map (~! filter (~! <<v equal? "#" 'o cdr)) 'o 
     map (~! <<n pts<-field 'o colist<-list) all-sea-monsters))

(def-thunk (! is-there-a-sea-monster-here? pt->sigil located-monster)
  (! CBN (~! colist<-list located-monster)
     % n> (~! cl-map (~ (λ (pt) (! pt->sigil 'get pt #f))))
     % n> (~! cl-map (~! equal? "#"))
     % n> all?
     % n$))

(def-thunk (! add-pt (list x1 y1) (list x2 y2))
  [x <- (! + x1 x2)] [y <- (! + y1 y2)]
  (ret (list x y)))

(def-thunk (! move-field pt pts) (! map (~! add-pt pt) pts))

(def-thunk (! count-safe-pts pt->sigil monster-pts)
  (! displayall "Hunting a new monster!" monster-pts)
  [field-pts <- (! <<v map first 'o pt->sigil 'to-list)]
  [max <- (! maximum (~! cl-map car (~! colist<-list field-pts)))]
  [safe-spots <- (! cl-foldl (~! cartesian-product (~! range 0 max) (~! range 0 max))
       (~ (λ (safe-spots pt)
            (do [monster <- (! move-field pt monster-pts)]
                (cond [(! is-there-a-sea-monster-here? pt->sigil monster)
                       (! foldl monster
                          (~ (λ (safe-spots unsafe-spot)
                               (! safe-spots 'remove unsafe-spot)))
                          safe-spots)]
                      [else (ret safe-spots)]))))
       pt->sigil)]
  (! <<v length 'o safe-spots 'to-list))

(def-thunk (! main-b f)
  [pt->sigil <- (! tbl<-field (~! parse f))]
  [total-spots <- (! <<v length 'o pt->sigil 'to-list)]
  (! displayall 'total-spots: total-spots)
  ;; (! <<v displayall 'o pt->sigil 'to-list )
  (! cl-foldr (~! colist<-list all-sea-monster-pts)
     (~ (λ (monster-pts k)
          (do [n <- (! count-safe-pts pt->sigil monster-pts)]
              (cond [(! = n total-spots) (! k)]
                    [else (ret (list 'roughness: n))]))))
     (~! error "no sea monsters found wtf")))

(provide main-b)
