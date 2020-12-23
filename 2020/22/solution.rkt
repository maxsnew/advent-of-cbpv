#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Eff
         fiddle/stdlib/Table
         fiddle/stdlib/IO
         "../../regex.rkt"
         )

(define! player/lex (! exact-string/lex "Player" 'player))
(define! space/lex (! char/lex ": \n" (~! abort 'space)))
(define sig (list player/lex space/lex unsigned-number/lex))

(def-thunk (! parse f)
   (! CBN (~! <<v fold-lex-string sig 'o (~! read-all-string f))
     % n> (~! cl-filter (~! <<v not 'o equal? 'space))
     % n> tl
     % n> (~! sep-by 'player)
     % n> list<-colist
     % n$))

(def-thunk (! declare-winner num deck disc)
  [csid <- (! reverse disc)]
  [win-deck <- (! append deck csid)]
  (ret (list 'winner num 'winners-deck: win-deck)))

(def/copat (! normalize)
  [('() disc)
   [csid <- (! reverse disc)]
   (ret (list csid '()))]
  [(deck disc)
   (ret (list deck disc))])

(def-thunk (! combat (list deck1 disc1) (list deck2 disc2))
  (pat (list deck1 deck2)
       [(list '() _) (! declare-winner 2 deck2 disc2)]
       [(list _ '()) (! declare-winner 1 deck1 disc1)]
       [(list (cons card1 bad-deck1) (cons card2 bad-deck2))
        (! displayall 'combat! card1 card2)
        (cond [(! < card1 card2)
               (! displayall "player 2 wins the round!")
               [player1 <- (! normalize bad-deck1 disc1)]
               [player2 <- (! normalize bad-deck2 (cons card1 (cons card2 disc2)))]
               (! combat player1 player2)]
              [else
               (! displayall "player 1 wins the round!")
               [player1 <- (! normalize bad-deck1 (cons card2 (cons card1 disc1)))]
               [player2 <- (! normalize bad-deck2 disc2)]
               (! combat player1 player2)])]))

(def-thunk (! final-score deck)
  [kced <- (! reverse deck)]
          (! cl-foldr (~! colist<-list kced)
             (~ (λ (x k multiplier)
                  (do [x*m <- (! * x multiplier)]
                      [multiplier <- (! + 1 multiplier)]
                    (! k multiplier x*m))))
             (~ (λ (multiplier) (! +)))
             1))

(def-thunk (! main-a f)
  (patc (! parse f)
  [(list (cons 1 deck1) (cons 2 deck2))
   (patc (! combat (list deck1 '()) (list deck2 '()))
         [(list 'winner player 'winners-deck: deck)
          (! final-score deck)])]))

(def/copat (! take-loop acc xs)
  [(0) (! reverse acc)]
  [(n)
   [n <- (! - n 1)]
   (pat xs [(cons x xs) (! take-loop (cons x acc) xs n)])])
(def-thunk (! take! n xs) (! take-loop '() xs n))

(def-thunk (! recursive-combat deck1 len1 deck2 len2 seen-games)
  (pat (list deck1 deck2)
  [(list deck1 '())
   (! displayall 'player 1 'wins! "(the old fashioned way)")
   (ret (list 1 deck1))]
  [(list '() deck2)
   (! displayall 'player 2 'wins! "(the old fashioned way)")
   (ret (list 2 deck2))]
  [(list (cons card1 deck1) (cons card2 deck2))
   [cur-state = (list (cons card1 deck1) (cons card2 deck2))]
   [new-seen-games <- (! seen-games 'set cur-state #t)]
   (cond [(! seen-games 'has-key? cur-state)
          (! displayall 'player 1 "wins! (by preventing a loop)")
          (ret (list 1 deck1))]
         [(! or (~! <= len1 card1) (~! <= len2 card2))
          ;; not enough cards for another sub-game
          (cond [(! > card1 card2)
                 ;; (! displayall 'player 1 'wins 'this 'round)
                 [deck1 <- (! append deck1 (list card1 card2))]
                 [len1 <- (! + len1 1)]
                 [len2 <- (! - len2 1)]
                 (! recursive-combat deck1 len1 deck2 len2 new-seen-games)]
                [else
                 ;; (! displayall 'player 2 'wins 'this 'round)
                 [deck2 <- (! append deck2 (list card2 card1))]
                 [len1 <- (! - len1 1)]
                 [len2 <- (! + len2 1)]
                 (! recursive-combat deck1 len1 deck2 len2 new-seen-games)])]
         [else ;; enough cards for a sub-game!
          (! displayall "time for a sub-game")
          [sub-deck1 <- (! take! card1 deck1)]
          [sub-deck2 <- (! take! card2 deck2)]
          (patc (! recursive-combat sub-deck1 card1 sub-deck2 card2 new-seen-games)
                ;; probably need to make the seen-games properly stateful :(
                [(list 1 _)
                 ;; (! displayall 'player 1 'wins 'this 'round)
                 [deck1 <- (! append deck1 (list card1 card2))]
                 [len1 <- (! + len1 1)]
                 [len2 <- (! - len2 1)]
                 (! recursive-combat deck1 len1 deck2 len2 new-seen-games)                 
                 ]
                [(list 2 _)
                 ;; (! displayall 'player 2 'wins 'this 'round)
                 [deck2 <- (! append deck2 (list card2 card1))]
                 [len1 <- (! - len1 1)]
                 [len2 <- (! + len2 1)]
                 (! recursive-combat deck1 len1 deck2 len2 new-seen-games)])])]))

(def-thunk (! main-b f)
  (patc (! parse f)
  [(list (cons 1 deck1) (cons 2 deck2))
   [len1 <- (! length deck1)]
   [len2 <- (! length deck2)]
   (patc (! recursive-combat deck1 len1 deck2 len2 empty-table)
         [(list player deck)
          (! displayall "final winner: " player)
          (! displayall "final deck: " deck)
          (! final-score deck)])]))

(provide main-a main-b)
