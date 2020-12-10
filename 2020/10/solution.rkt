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

;; for part b we need to check if arrangements are valid
(require fiddle/stdlib/Eff)

(def/copat (! last)
  [((cons x '())) (ret x)]
  [((cons x xs)) (! last xs)])

;; effects
(def-thunk (! choose t1 t2)
  (! bindE (~! raiseE 'flip)
     (~ (copat [(#t) (! t1)]
               [(#f) (! t2)]))))

;; valid-chain? : Nat -> DescendingListof Nat -> NonDet Bool
(def/copat (! valid-chain? goal)
  [('())
   (! mapE (~! <= goal 3))]
  [((cons next xs))
   ;; (! displayall 'hello goal next xs)
   [diff <- (! - goal next)]
   (cond [(! <= diff 3)
          (! appE^ (~! swap valid-chain? xs) (~! choose (~! retE goal) ;; drop this one
                                                        (~! retE next) ;; keep this one
                                                        ))]
         [else
          ;; if this gap is too big, dropping this one will only make it bigger
          (! retE #f)])])

(def-thunk (! counts-with-gap next goal goal-count)
  [diff <- (! - goal next)]
  (cond [(! <= diff 3)
         (ret goal-count)]
        [else (ret 0)]))

;; valid-chains
(def-thunk (! valid-chains goal l)
  [hugenum <- (! + goal 7)]
  [final <- (! foldl l
       (~ (copat
           [((list (list count3 num3) (list count2 num2) (list count1 num1)) num0)
            [count0 <- (! idiom^ + (~! counts-with-gap num0 num3 count3)
                          (~! counts-with-gap num0 num2 count2)
                          (~! counts-with-gap num0 num1 count1))]
            ;; (! displayall 'count0: num0 count0)
            (ret (list (list count2 num2) (list count1 num1) (list count0 num0)))]))
       (list (list 0 hugenum) (list 0 hugenum) (list 1 goal)))]
  (! <<v first 'o third final))

(def-thunk (! main-b f)
  [inp <- (! <<v swap sort > 'o list<-colist (~! parse-file f))]
  [goal <- (! <<v + 3 'o car inp)]
  (! handle (~! valid-chain? goal inp)
     (~ (copat [(#t n) (! + 1 n)]
               [(#f n) (ret n)]))
     (~ (copat
         [('flip resume n)
          [n <- (! resume #t n)]
          (! resume #f n)]))
     0))

(def-thunk (! fast-main-b f)
  [inp <- (! <<v swap sort > 'o Cons 0 'o list<-colist (~! parse-file f))]
  [goal <- (! <<v + 3 'o car inp)]
  ;; (! handle (~! valid-chain? goal inp)
  ;;    (~ (copat [(#t n) (! + 1 n)]
  ;;              [(#f n) (ret n)]))
  ;;    (~ (copat
  ;;        [('flip resume n)
  ;;         [n <- (! resume #t n)]
  ;;         (! resume #f n)]))
  ;;    0)
  (! valid-chains goal inp)
  )

(! <<v displayall 'o main-a "full.txt")
(! <<v displayall 'o fast-main-b "full.txt")
