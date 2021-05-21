;sat

(set-logic QF_S)
(set-logic QF_LIA)
(declare-fun x () String)
(declare-fun y () String)
(declare-fun z () String)

;(assert (str.in.re y (re.+(str.to.re "yy"))))
;(assert (= z (str.++ x y)))
;(assert (= 5 (str.len z)))
(assert (str.in.re x (str.to.re "y")))
(assert (< 1 (str.len x)))

(check-sat)
