(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(assert (str.in.re sigmaStar_0 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof sigmaStar_0  "\x61" 0)) (str.len sigmaStar_0 )))
(check-sat)
(get-model)
