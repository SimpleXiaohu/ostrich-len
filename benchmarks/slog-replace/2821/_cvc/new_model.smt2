(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_7 () String)
(declare-fun sigmaStar_8 () String)
(declare-fun sigmaStar_9 () String)
(declare-fun literal_10 () String)
(declare-fun x_11 () String)
(declare-fun literal_12 () String)
(declare-fun x_13 () String)
(declare-fun literal_14 () String)
(declare-fun x_15 () String)
(declare-fun sigmaStar_20 () String)
(declare-fun x_23 () String)
(declare-fun literal_3 () String)
(assert (= literal_10 "\x2f\x44\x65\x66\x61\x75\x6c\x74\x2e\x68\x74\x6d"))
(assert (= x_11 (str.++ sigmaStar_7 literal_10)))
(assert (= literal_12 "\x2f\x69\x6e\x64\x65\x78\x2e\x68\x74\x6d\x6c"))
(assert (= x_13 (str.++ sigmaStar_8 literal_12)))
(assert (= literal_14 "\x2f\x69\x6e\x64\x65\x78\x2e\x68\x74\x6d"))
(assert (= x_15 (str.++ sigmaStar_9 literal_14)))
(assert (= literal_3 "\x66\x69\x6c\x65\x2e\x70\x68\x70"))
(assert (or (= x_23 x_11) (= x_23 sigmaStar_20) (= x_23 literal_3) (= x_23 x_15) (= x_23 x_13)))
(assert (str.in.re x_23 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_23 "\x61" 0)) (str.len x_23)))
(check-sat)
(get-model)
