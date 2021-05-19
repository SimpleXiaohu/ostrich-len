(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_6 () String)
(declare-fun sigmaStar_8 () String)
(declare-fun sigmaStar_10 () String)
(declare-fun literal_11 () String)
(declare-fun x_7 () String)
(declare-fun x_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_14 () String)
(assert (= x_7 (str.replace sigmaStar_1 "\x2f\x2e\x28\x5c\x64\x2b\x29\x2e\x2f" "\x5f\x24\x31\x2e")))
(assert (= literal_11 "\x20\x20\x20\x20"))
(assert (= x_12 (str.++ literal_11 sigmaStar_10)))
(assert (= literal_13 "\x2e\x73\x63\x6f\x72\x65\x2e\x6d\x61\x78\x20\x3d\x20\x27\x27\x3b\x5c\x6e"))
(assert (= x_14 (str.++ x_12 literal_13)))
(assert (str.in.re x_14 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_14 "\x61" 0)) (str.len x_14)))
(check-sat)
(get-model)
