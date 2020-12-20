#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Eff
         fiddle/stdlib/Table
         fiddle/stdlib/IO
         "../../regex.rkt"
         )


(define! tile/lex (! exact-string/lex "Tile"))
(define! space/lex (! char/lex ": " (~! abort 'space)))
(define! newline/lex (! char/lex "\n" (~! abort 'newline)))
(define! sigil/lex (! char/lex "#." list->string))
(define sig (list unsigned-number/lex tile/lex space/lex newline/lex sigil/lex))

(def-thunk (! parse f)
   (! CBN (~! <<v fold-lex-string sig 'o (~! read-all-string f))
     % n> (~! cl-filter (~! <<v not 'o equal? 'space))
     % n> (~! sep-by "Tile")
     % n> (~! cl-map (~! <<n list<-colist 'o sep-by 'newline 'o colist<-list))
     % n> tl
     % n> (~! cl-map (~! <<v reverse 'o rest 'o reverse))
     % n> (~! cl-map (~ (copat [((cons (list id) lines)) (ret (cons id lines))])))
     % n$))

;; Dihedral group of order 4 describes the symmetries of a non-square rectangle.
;
;; Normal forms are a pair of a rotation (2 possible) and whether or not it's flipped (2 possible)

;; two generators: a is rotate pi radians, b is flip across y axis
;
;; rotations: 0, 'pi
;; flipped:   'r, 'l

;; Plan: Greedy solution (check if it will work first)
;; parse into a table of id -> Piece?

;; Make 2 tables of
;;    top side   -> id * orientation     The orientation is how to orient the piece to make the given side the top side
;;    right side -> id * orientation     Orientation is how to orient the piece to make the given side the right side

;; Find the corners: The 4 pieces that are missing a matching top and right side.
;
;; This is all that's needed for part a!


;; Select one at random, find a piece that matches one of its sides,
;; add it to the puzzle.

;; How to represent the puzzle?

;; A RawPiece is an ID Listof 

;; A Piece consists of
;;   - A bitmap
;;   - top edge
;;   - bottom edge
;;   - left edge
;;   - right edge

;; An Orientation consists of
;;   - a rotation in 0, pi/2, pi, minus-pi/2
;;   - a handedness in r or l

;; An OrientedPiece is a Orientation * Piece

;
;; A Puzzle consists of
;;   - piece-at : Table Pt OrientedPiece
;;   - frontier : Listof Pt

;; Listof (Listof Sigil) -> Listof (List Top-Piece Orientation) 
(def-thunk (! mk-edges lines)
  [top <- (! first lines)]
  [pot <- (! reverse top)]

  [bot <- (! last lines)]
  [tob <- (! reverse bot)]

  [left <- (! map first lines)]
  [tfel <- (! reverse left)]

  [right <- (! map last lines)]
  [thgir <- (! reverse right)]
  
  (ret (list (list top (list   0 'r))
             (list pot (list   0 'l))

             (list right (list 'pi/2 'r))
             (list thgir (list 'pi/2 'l))

             (list bot (list 'pi 'l))
             (list tob (list 'pi 'r))

             (list left (list 'minus-pi/2 'l))
             (list tfel (list 'minus-pi/2 'r)))))

(def-thunk (! fill-db db id oriented-edges)
  (! foldl oriented-edges 
     (~ (copat [(db (list edge orientation))
                (! db 'set (list edge (list id orientation)) #t)]))
     db))

;; Listof (Cons ID BitMap) -> F(TableSet (List TopEdge (List ID Orientation)))
(def-thunk (! mk-edge-db pieces)
  (! foldl pieces
     (~ (copat [(tops (cons id lines))
                [oriented-tops <- (! mk-edges lines)]
                (! fill-db tops id oriented-tops)]))
     empty-table))

(def/copat (! single?)
  [((list x)) (ret #t)]
  [(_) (ret #f)])

;; ;; 
(def-thunk (! find-edges good-rots a->bs)
  [good-rots~ = (~! colist<-list good-rots)]
  (! CBV  (~! a->bs 'to-list)
     % v> (~! map cdr)
     % v> (~! filter single?)
     % v> (~! map first)
     % v> (~! filter (~! <<v swap member? good-rots~ 'o first 'o second))
     % v$))

;; Make a 
(def-thunk (! main-a f)
  [id-lines <- (! list<-colist (~! parse f))]
  ;; (! displayall id-lines)
  [tops <- (! mk-edge-db id-lines)]
  [top-side->pieces <- (! <<v first 'o split-adjacency-tbl tops)]
  [top-edges <- (! <<v table-set<-list 'o map first 'o find-edges '(0 pi) top-side->pieces)]
  [right-edges <- (! <<v table-set<-list 'o map first 'o find-edges '(pi/2 minus-pi/2) top-side->pieces)]
  ;; (! <<v displayall 'top-edges: 'o top-edges 'to-list)
  ;; (! <<v displayall 'right-edges: 'o right-edges 'to-list)
  [corners <- (! <<v map first 'o swap $ 'to-list 'o table-set-intersect top-edges right-edges)]
  (! <<v displayall 'corners: corners)
  (! foldl corners * 1 ))
;; (define! l->rs (! main-a "sample"))

(def-thunk (! solve-puzzle f)
  [id-lines <- (! list<-colist (~! parse f))]
  ;; (! displayall id-lines)
  [tops <- (! mk-edge-db id-lines)]
  [top-side->pieces <- (! <<v first 'o split-adjacency-tbl tops)]

  [oriented-top-edges <- (! find-edges '(0 pi) top-side->pieces)]
  [top-edge-ids <- (! <<v table-set<-list 'o map first oriented-top-edges)]
  [oriented-right-edges <- (! find-edges '(pi/2 minus-pi/2) top-side->pieces)]
  [right-edge-ids <-  (! <<v table-set<-list 'o map first oriented-right-edges)]
  [corners <- (! <<v map first 'o swap $ 'to-list 'o table-set-intersect top-edge-ids right-edge-ids)]
  (! <<v displayall 'corners: corners)
  [disoriented-top-left <- (! first corners)]
  (! displayall 'oriented: oriented-top-edges)
  (ret disoriented-top-left))

(def-thunk (! main-b f)
  (! error 'nyi)
  )

(provide main-a main-b solve-puzzle)
