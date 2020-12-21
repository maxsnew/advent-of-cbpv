#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Table)

(provide topo-sort)



;; The initial frontier is all the vertices with no antecedents
(def-thunk (! initial-frontier ante->succs)
  (! CBV (~! ante->succs 'to-list)
     % v> (~! filter (~ (copat [((cons ante succs)) (! null? succs)])))
     % v> (~! map car)
     % v$))

;; Kahn's algorithm
;; SortedListof Vertex -> Listof Vertex -> Table Vertex (Listof Vertex) -> Table Vertex (Listof Vertex) -> SortedListof Vertex
(def-thunk (! Kahn sorted frontier ante->succs succ->antes)
  (cond [(! empty? frontier) (! reverse sorted)]
        [else
         [next <- (! first frontier)] [frontier <- (! rest frontier)]
         [succs <- (! ante->succs 'get next '())]
         [remove-dependency = (~ (λ (acc succ) (do
          [frontier <- (! car acc)]
          [succ->antes <- (! cdr acc)]
          [remove-next = (~ (! filter (~ (! <<v not 'o equal? next))))]
          [succ->antes <- (! update succ->antes succ '() remove-next)]
          [frontier <- (ifc (! <<v empty? 'o succ->antes 'get succ '())
                            (! Cons succ frontier)
                            (ret frontier))]
          (! Cons frontier succ->antes))))]
         [updated <-
                  (! cl-foldl^ remove-dependency
                     (cons frontier succ->antes)
                     (~ (! colist<-list succs)))]
         [frontier <- (! car updated)] [succ->antes <- (! cdr updated)]
         [sorted <- (! Cons next sorted)]
         (! Kahn sorted frontier ante->succs succ->antes)]))

;; Table (list A B) Bool -> List (Table A (Listof B)) (Table B (Listof A))
(def-thunk (! split-adjacency-tbl^ rel)
  [init-tbl <- (! <<v table<-list 'o map (~! swap Cons empty-table) 'o set->list 'o list->set 'o apply append 'o map car 'o rel 'to-list)]
  (! cl-foldr (~! <<v colist<-list 'o rel 'to-list)
     (~ (copat [((cons (list l r) _) k l->rs r->ls)
                [l->rs <- (! push-tbl l->rs l r)]
                [r->ls <- (! push-tbl r->ls r l)]
                (! k l->rs r->ls)]))
     (~ (λ (l->rs r->ls) (! map (~! map-vals (~! <<v map car 'o swap apply '(to-list))) (list l->rs r->ls))))
     init-tbl
     init-tbl))

;; U(TblSet (List V V)) -> F (Listof V)
(def-thunk (! topo-sort adj)
  (patc (! split-adjacency-tbl^ adj)
        [(list ante->succs succ->antes)
         [frontier <- (! initial-frontier ante->succs)]
         (! Kahn '() frontier succ->antes ante->succs)]))

(define! example-2020-19
  (! table-set<-list '((0 4) (0 1) (0 5)  (1 2) (1 3)  (2 4) (2 5)  (3 4) (3 5))))


