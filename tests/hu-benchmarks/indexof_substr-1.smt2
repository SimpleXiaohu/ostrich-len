;sat

(set-logic QF_S)
(declare-fun x () String)
(declare-const t1 Int)


(assert(= t1 (- 0 1)))
(assert(str.in.re x (str.to.re "abcabc")))
(assert(= (str.indexof x "abd" 0) t1))
(assert(= (str.indexof x "abc" 0) 0))
(assert (= "bca" (str.substr x 1 3)))

(check-sat)
