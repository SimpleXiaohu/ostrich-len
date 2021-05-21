(set-logic QF_S)
(set-logic QF_LIA)
(declare-fun x () String)

(assert(str.in.re x (str.to.re "abcabc")))
;(assert(= (str.indexof x "abc" 1) 3))   ;sat	
;(assert(= (str.indexof x "abc" 0) 0))	;sat
(assert(= (str.indexof x "abc" 1) 0))	;unsat

(check-sat)
