(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_7 () String)
(declare-fun epsilon () String)
(declare-fun literal_11 () String)
(declare-fun x_8 () String)
(declare-fun x_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_14 () String)
(assert (= epsilon ""))
(assert (= x_8 (str.replace epsilon "\x2f\x5b\x2e\x7c\x20\x5d\x2f" "\x5f")))
(assert (= literal_11 "\x3c\x63\x65\x6e\x74\x65\x72\x3e\x3c\x66\x6f\x6e\x74\x20\x63\x6f\x6c\x6f\x72\x3d\x22\x72\x65\x64\x22\x3e\x6e\x6f\x6e\x75\x6d\x65\x72\x69\x63\x77\x65\x69\x67\x68\x74\x3a\x20\x22"))
(assert (= x_12 (str.++ literal_11 x_8)))
(assert (= literal_13 "\x22\x3c\x2f\x66\x6f\x6e\x74\x3e\x3c\x2f\x63\x65\x6e\x74\x65\x72\x3e\x3c\x62\x72\x20\x2f\x3e"))
(assert (= x_14 (str.++ x_12 literal_13)))
(assert (str.in.re x_14 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_14 "\x61" 0)) (str.len x_14)))
(check-sat)
(get-model)
