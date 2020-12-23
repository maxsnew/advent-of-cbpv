#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Eff
         fiddle/stdlib/Table
         fiddle/stdlib/IO
         "../../regex.rkt"
         )

(define sample '(3 8 9 1 2 5 4 6 7))
(define input '(3 2 7 4 6 5 1 8 9))

(def-thunk (! wrap n)
  (cond [(! < n 1) (! + n 9)]
        [else (ret n)]))

(def-thunk (! ins-loop dest lifted (cons x xs))
  (cond [(! = dest x) (! append (cons dest lifted) xs)]
        [else
         [xs <- (! ins-loop dest lifted xs)]
         (ret (cons x xs))]))

(def-thunk (! cup-insert n lifted cups)
  [dest <- (! <<v first 'o filter (~! <<v not 'o swap member? (~! colist<-list lifted)) 'o map wrap 'o map (~! - n) '(1 2 3 4))]
  ;; (! displayall "cup: " n)
  ;; (! displayall "lifted: " lifted)
  ;; (! displayall "dest: " dest)
  ;; (! displayall "remaining: " cups)
  (! ins-loop dest lifted cups))

(def/copat (! a-loop)
  [(0 state) (ret state)]
  [(n (cons cur (cons x1 (cons x2 (cons x3 cups)))))
   [n <- (! - n 1)]
   [cups <- (! cup-insert cur (list x1 x2 x3) cups)]
   [cups <- (! append cups (list cur))]
   (! <<v a-loop n 'o debug 'cups: cups)])

(def-thunk (! find-and-drop n acc (cons x xs))
  (cond [(! = n x) (! <<v append xs 'o reverse acc)]
        [else (! find-and-drop n (cons x acc) xs)]))

(def-thunk (! main-a start)
  (! <<v apply string-append 'o map number->string 'o find-and-drop 1 '() 'o a-loop 100 start))

(def-thunk (! main-b f)
  (! error 'nyi)
  )

(provide main-a main-b sample input)
