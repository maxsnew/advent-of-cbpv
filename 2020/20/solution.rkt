#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Eff
         fiddle/stdlib/Table
         fiddle/stdlib/IO
         "../../regex.rkt"
         "lib.rkt"
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

;; Dihedral group of order 8 describes the symmetries of a square
;
;; Normal forms are a pair of a rotation (4 possible) and whether or not it's flipped (2 possible)

;; two generators: a is rotate pi radians, b is flip across y axis
;
;; rotations: 0, 'pi, 'pi/2, 'minus-pi/2
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

;; An Orientation consists of
;;   - a rotation in 0, pi/2, pi, minus-pi/2
;;   - a handedness in r or l

;; Interpreted geometrically, this is interpreted as
;; Flip about the y axis if it's 'l, then rotate by the angle

;; An OrientedPiece is a Orientation * Piece
;
;; A Puzzle consists of
;;   - piece-at : Table Pt OrientedPiece
;;   - frontier : Listof Pt

;; Listof (Listof Sigil) -> Listof (List Top-Piece Orientation) 
(def-thunk (! mk-edges lines)
  (! map (~ (λ (o)
              (do [e <- (! top-edge lines o)]
                  (ret (list e o)))))
     all-orientations))

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

;; Table Id BitMap -> OrientedPiece -> Line
(def-thunk (! lookup-top-edge id->field (list id o))
  [field <- (! id->field 'get id #f)]
  (! top-edge field o))

(def-thunk (! lookup-left-edge id->field opiece)
  [opiece <- (! orient-piece '(pi/2 l) opiece)]
  (! lookup-top-edge id->field opiece))

;; Table ID Field -> Table Line (Listof OrientedPiece) -> OrientedPiece -> OrientedPiece
(def-thunk (! select-to-the-left id->field top-side->pieces right-piece)
  ;; (! displayall 'looking-to-the-left-of right-piece)
  [right-id <- (! first right-piece)]
  [edge <- (! lookup-left-edge id->field right-piece)]
  [next-edge <- (! <<v orient-piece (list 'minus-pi/2 'r) 'o first
       'o (~! filter (~! <<v not 'o = right-id 'o first))
       'o top-side->pieces 'get edge #f)]
  (ret next-edge))

(def-thunk (! select-above id->field top-side->pieces below-piece)
  [diso-below-piece <- (! orient-piece '(pi/2 r) below-piece)]
  [diso-above-piece <- (! select-to-the-left id->field top-side->pieces diso-below-piece)]
  (! orient-piece '(minus-pi/2 r) diso-above-piece))

;;
;; We fill in the puzzle row by row, from bottom to top, each row from
;; right to left.
;
;; 
(def-thunk (! fill-in-rows id-tbl top-side->pieces side-length leftmost-piece cur-row col-num prev-rows row-num)
  (cond [(! = col-num 1)
         [cur-row = (cons leftmost-piece cur-row)]
         [prev-rows = (cons cur-row prev-rows)]
         (cond [(! = row-num 1) (ret prev-rows)]
               [else
                [row-num <- (! - row-num 1)]
                [below-leftmost-piece <- (! last cur-row)]
                [leftmost-piece <- (! select-above id-tbl top-side->pieces below-leftmost-piece)]
                ;; (! displayall 'new-row-starting-with: leftmost-piece)
                (! fill-in-rows id-tbl top-side->pieces side-length leftmost-piece '() side-length prev-rows row-num)])]
        [else
         [col-num <- (! - col-num 1)]
         [cur-row = (cons leftmost-piece cur-row)]
         [next-piece <- (! select-to-the-left id-tbl top-side->pieces leftmost-piece)]
         ;; (! displayall 'next-piece: next-piece)
         (! fill-in-rows id-tbl top-side->pieces side-length next-piece cur-row col-num prev-rows row-num)]))

;; Table -> OrientedPiece -> Listof (Listof 1String)
(def-thunk (! orient-opiece id->piece (list id o))
  [field <- (! id->piece 'get id #f)]
  (! orient-rows o field))

(def-thunk (! middle xs)
  (! <<v rest 'o reverse 'o rest 'o reverse xs))

(def-thunk (! trim rows)
  (! <<v middle 'o map middle rows))

;; Listof (Listof String) -> Listof String
(def-thunk (! append-fields fields)
  (! multi-map (~! string-append) fields))

;; Table ID Field -> Listof (Listof OrientedPiece) -> String
(def-thunk (! string<-solved id->piece id-puzzle)
  ;; : Listof (Listof (Listof String)) ~ Rows (PieceFields)
  [piece-fields <- (! map (~! map (~! <<v trim 'o orient-opiece id->piece)) id-puzzle)]

  (! <<v
     apply string-append 'o
     map (~! apply string-append) 'o
     map (~! swap append '("\n")) 'o
     apply append 'o map (~! multi-map append) piece-fields)
  #;
  (! <<v
     ;;append-fields 'o

      ;; map (~! apply string-append) 'o
     ;; Rows (Listof Line)
     map append-fields piece-fields)
  )

(def-thunk (! solve-puzzle f)
  [id-lines <- (! list<-colist (~! parse f))]
  [side-len <- (! <<v swap expt 1/2 'o length id-lines)]
  ;; (! displayall 'length: side-len)
  ;; (! displayall id-lines)
  [tops <- (! mk-edge-db id-lines)]
  [top-side->pieces <- (! <<v first 'o split-adjacency-tbl tops)]

  [oriented-top-edges <- (! find-edges '(0 pi) top-side->pieces)]
  [top-edge-ids <- (! <<v table-set<-list 'o map first oriented-top-edges)]
  [oriented-right-edges <- (! find-edges '(pi/2 minus-pi/2) top-side->pieces)]
  [right-edge-ids <-  (! <<v table-set<-list 'o map first oriented-right-edges)]
  [corners <- (! <<v map first 'o swap $ 'to-list 'o table-set-intersect top-edge-ids right-edge-ids)]

  [bot-right-id <- (! first corners)]
  [br-tops <- (! <<v first 'o map second 'o
                 filter (~! <<v equal? 'r 'o second 'o second) 'o
                 filter (~! <<v equal? bot-right-id 'o first) oriented-top-edges)]
  [br-rights <- (! <<v first 'o map second 'o
                   filter (~! <<v equal? 'pi/2 'o first 'o second) 'o
                   filter (~! <<v equal? bot-right-id 'o first) oriented-right-edges)]

  [top-rot <- (! first br-tops)]
  [side-flip <- (! second br-rights)]
  [br-o <- (pat (list top-rot side-flip)
                [(list 0 'r) (ret (list 'pi 'l))]
                [(list 0 'l) (ret (list 'pi 'r))]
                [(list 'pi 'l) (ret (list 0 'l))]
                [(list 'pi 'r) (ret (list 'pi 'r))])]

  ;; this is the orientation of bot-right-id to make it the bottom right tile
  [bot-right = (list bot-right-id br-o)]
  [id-tbl <- (! table<-list id-lines)]
  [solved <- (! fill-in-rows id-tbl top-side->pieces side-len bot-right '() side-len '() side-len)]
  [solved-field <- (! string<-solved id-tbl solved)]
  (ret solved-field))


(provide main-a solve-puzzle)
