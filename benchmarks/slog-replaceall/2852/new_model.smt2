(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun x_4 () String)
(declare-fun literal_5 () String)
(declare-fun x_6 () String)
(assert (= x_4 (str.replaceall sigmaStar_0 "\x23\x5b\x5c\x7c\x2f\x5d\x23" "")))
(assert (= literal_5 "\x2e\x70\x68\x70"))
(assert (= x_6 (str.++ x_4 literal_5)))
(assert (str.in.re x_6 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (> (* 2 (str.indexof x_6 "\x61" 0)) (str.len x_6)))
(check-sat)
(get-model)