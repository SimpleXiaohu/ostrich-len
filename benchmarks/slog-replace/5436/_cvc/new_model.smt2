(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun x_3 () String)
(declare-fun literal_2 () String)
(assert (= literal_2 "\x63\x6f\x75\x6e\x74"))
(assert (or (= x_3 literal_2) (= x_3 sigmaStar_1)))
(assert (str.in.re x_3 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_3 "\x61" 0)) (str.len x_3)))
(check-sat)
(get-model)
