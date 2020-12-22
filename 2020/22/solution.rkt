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

(def-thunk (! main-a f)
  (patc (! parse f)
  [(list (cons 1 deck1) (cons 2 deck2))
   (patc (! combat (list deck1 '()) (list deck2 '()))
         [(list 'winner player 'winners-deck: deck)
          [kced <- (! reverse deck)]
          (! cl-foldr (~! colist<-list kced)
             (~ (λ (x k multiplier)
                  (do [x*m <- (! * x multiplier)]
                      [multiplier <- (! + 1 multiplier)]
                    (! k multiplier x*m))))
             (~ (λ (multiplier) (! +)))
             1)])]))

(def-thunk (! main-b f)
  (! error 'nyi)
  )

(provide main-a main-b)
