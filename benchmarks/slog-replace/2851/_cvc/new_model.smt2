(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun literal_4 () String)
(declare-fun x_5 () String)
(assert (= literal_4 "\x20\x6b\x42"))
(assert (= x_5 (str.++ sigmaStar_3 literal_4)))
(assert (str.in.re x_5 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_5 "\x61" 0)) (str.len x_5)))
(check-sat)
(get-model)
