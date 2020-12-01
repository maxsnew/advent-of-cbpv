#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/IO)

(provide a-input a-input~ sample-input sample-input~
         a-soln a-soln^
         b-soln b-soln^
         faster-b
         faster-faster-b)

(define sample-input '(1721 979 366 299 675 1456))
(def-thunk (! sample-input~) (! colist<-list '(1721 979 366 299 675 1456)))
(define a-input '(2004 1823 1628 1867 1073 1951 1909 1761 1093 1992 1986 1106 1537 1905 1233 1961 1760 1562 1781 1329 1272 1660 1367 1248 1697 1515 1470 1980 1884 1784 1966 1778 1426 1255 1089 1748 1253 1870 1651 1131 1623 1595 1128 1014 1863 1855 1203 1395 1521 1365 1202 780 1560 1834 1494 1551 1398 1190 1975 1940 1217 1793 1310 1070 1865 1307 1735 1897 1410 1994 1541 1569 1731 1238 1193 1226 1435 1159 1642 1652 1908 1920 1930 1068 1914 1186 1795 1888 1634 1750 1950 1493 1353 1461 1658 1856 1301 1538 1948 1998 1847 1880 1657 1536 1457 1762 1706 1894 542 1991 1108 1072 1064 1511 1496 1480 1955 1604 1766 1983 1713 1234 1503 1583 1729 1140 1006 1600 1699 1280 1891 1996 1375 1167 1625 1129 1770 1497 1620 1267 1421 1399 1563 1636 1293 1506 1613 1958 1967 1182 1050 1947 1787 1774 1928 1896 1303 1826 1132 1254 1752 1510 1705 1229 1558 1989 1567 698 1738 1357 1587 1316 1838 1311 1057 1644 1135 1300 1134 1577 1381 1806 1176 1993 1769 1633 1450 1819 1973 1694 969 1987 1095 1717 1933 1593 1045 1355 1459 1619))
(def-thunk (! a-input~) (! colist<-list a-input))

(def-thunk (! a-soln inp~)
  (patc (! CBN
           (~! cartesian-product inp~ inp~) % n>
           (~! cl-filter (~ (copat [((list x y)) (! CBV (~! + x y) % v> (~! equal? 2020) % v$)]))) % n>
           $ % n$)
        [(list 'cons (list x y) _) (! * x y)]))

(def-thunk (! b-soln inp~)
  (patc (! CBN
           (~! cartesian-product inp~ (~! cartesian-product inp~ inp~)) % n>
           (~! cl-filter (~ (copat [((list x (list y z))) (! CBV (~! + x y z) % v> (~! equal? 2020) % v$)]))) % n>
           $ % n$)
        [(list 'cons (list x (list y z)) _) (! * x y z)]))

(def-thunk (! faster-b inp~)
  (patc (! CBN
           (~! cartesian-product inp~ inp~) % n>
           (~! cl-filter (~ (copat [((list x y)) (! CBV (~! + x y) % v> (~! >= 2020) % v$)]))) % n>
           (~! cartesian-product inp~) % n>
           (~! cl-filter (~ (copat [((list x (list y z))) (! CBV (~! + x y z) % v> (~! = 2020) % v$)]))) % n>
           $ % n$
           )
        [(list 'cons (list x (list y z)) _) (! * x y z)]))

(def-thunk (! b-init-filter inp~)
  [p? =
      (~ (copat
          [((list x y))
           (! and (~! < x y)
                  (~! CBV (~! + x y) % v> (~! >= 2020) % v$))]))]
  (! CBN
     (~! cartesian-product inp~ inp~) % n>
     (~! cl-filter p?) % n$))

(def-thunk (! faster-faster-b inp~)
  (patc (! CBN
     (~! cartesian-product inp~ (~! b-init-filter inp~)) % n>
     (~! cl-filter (~ (copat [((list x (list y z))) (! CBV (~! + x y z) % v> (~! = 2020) % v$)]))) % n>
     $ % n$)
        [(list 'cons (list x (list y z)) _) (! * x y z)]))

;; 

(def/copat (! uniq-looples l)
  [(0) (! cl-single '())]
  [(n)
   [n-1 <- (! - n 1)]
   (! cl-bind (~! tails l)
     (~ (copat [((cons hd xs))
                (! cl-map (~! Cons hd) (~! uniq-looples xs n-1))]
               [('()) (! cl-nil)])))])
(def-thunk (! uniq-tuples l n)
  (! uniq-looples l n))

(def-thunk (! uniq-pairs l) (! uniq-tuples l 2))
(def-thunk (! uniq-triples l) (! uniq-tuples l 3))

(def-thunk (! soln inp tup-size)
  [inp <- (! sort inp <)]
  (patc (! CBN
           (~! uniq-tuples inp tup-size) % n>
           (~! cl-filter (~ (copat [(xs) (! CBV (~! apply + xs) % v> (~! equal? 2020) % v$)]))) % n>
           $ % n$)
        [(list 'cons xs _) (! apply * xs)]))

(def-thunk (! a-soln^ inp) (! soln inp 2))
(def-thunk (! b-soln^ inp) (! soln inp 3))
