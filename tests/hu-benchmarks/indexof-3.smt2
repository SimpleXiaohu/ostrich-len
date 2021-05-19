;sat

(set-logic QF_S)
(set-logic QF_LIA)
(declare-fun x1 () String)
(declare-fun x2 () String)
(declare-fun x3 () String)
(declare-const t Int)


(assert(str.in.re x1 (re.* (str.to.re "ab"))))
(assert(str.in.re x2 (re.* (str.to.re "cd"))))
(assert(= x3 (str.++ x1 x2)))
(assert(= (str.indexof x3 "abacd" 10000) t))	
(assert (< t 10000000))

(check-sat)
