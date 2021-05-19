(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_2 () String)
(declare-fun x_3 () String)
(assert (= literal_2 "\x2f\x2e\x2e\x2f\x63\x6f\x6e\x66\x69\x67\x2e\x70\x68\x70"))
(assert (= x_3 (str.++ sigmaStar_1 literal_2)))
(assert (str.in.re x_3 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_3 "\x61" 0)) (str.len x_3)))
(check-sat)
(get-model)
