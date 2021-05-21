(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun literal_4 () String)
(declare-fun x_7 () String)
(declare-fun sigmaStar_9 () String)
(declare-fun x_8 () String)
(declare-fun literal_6 () String)
(declare-fun literal_10 () String)
(declare-fun x_11 () String)
(declare-fun literal_3 () String)
(declare-fun x_12 () String)
(declare-fun literal_18 () String)
(assert (= literal_4 "\x2f\x6c\x61\x6e\x67\x2f"))
(assert (= x_7 (str.++ literal_4 sigmaStar_2)))
(assert (= literal_6 "\x2f\x6c\x61\x6e\x67\x2f\x65\x6e\x5f\x75\x74\x66\x38"))
(assert (or (= x_8 x_7) (= x_8 literal_6)))
(assert (= literal_10 "\x2f\x68\x65\x6c\x70\x2f"))
(assert (= x_11 (str.++ x_8 literal_10)))
(assert (= literal_3 "\x2f\x6c\x61\x6e\x67\x2f\x65\x6e\x5f\x75\x74\x66\x38\x2f\x68\x65\x6c\x70"))
(assert (= x_12 (str.++ x_11 literal_3)))
(assert (= literal_18 "\x3c\x70\x3e\x3c\x66\x6f\x6e\x74\x20\x63\x6f\x6c\x6f\x72\x3d\x22\x72\x65\x64\x22\x3e\x66\x69\x6c\x65\x6d\x69\x73\x73\x69\x6e\x67\x3c\x2f\x66\x6f\x6e\x74\x3e\x3c\x2f\x70\x3e"))
(assert (str.in.re literal_18 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_12 "\x61" 0)) (str.len x_12)))
(check-sat)
(get-model)
