(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_4 () String)
(declare-fun literal_5 () String)
(declare-fun x_9 () String)
(declare-fun literal_6 () String)
(declare-fun x_10 () String)
(declare-fun literal_7 () String)
(declare-fun x_12 () String)
(declare-fun literal_8 () String)
(declare-fun x_13 () String)
(declare-fun literal_11 () String)
(declare-fun x_14 () String)
(declare-fun x_15 () String)
(declare-fun x_16 () String)
(declare-fun x_17 () String)
(declare-fun x_18 () String)
(declare-fun x_21 () String)
(declare-fun x_22 () String)
(declare-fun literal_23 () String)
(declare-fun x_27 () String)
(declare-fun x_28 () String)
(declare-fun literal_29 () String)
(declare-fun x_31 () String)
(declare-fun literal_30 () String)
(declare-fun x_32 () String)
(declare-fun x_33 () String)
(assert (= literal_5 "\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x5c\x6e"))
(assert (= x_9 (str.++ sigmaStar_3 literal_5)))
(assert (= literal_6 "\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x5c\x6e"))
(assert (= x_10 (str.++ sigmaStar_0 literal_6)))
(assert (= literal_7 "\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x5c\x6e"))
(assert (= x_12 (str.++ sigmaStar_4 literal_7)))
(assert (= literal_8 "\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x5c\x6e"))
(assert (= x_13 (str.++ sigmaStar_1 literal_8)))
(assert (= literal_11 ""))
(assert (= x_14 (str.++ literal_11 x_13)))
(assert (or (= x_15 literal_11) (= x_15 x_14)))
(assert (= x_16 (str.++ x_15 x_9)))
(assert (or (= x_17 literal_11) (= x_17 x_14)))
(assert (= x_18 (str.++ x_17 x_10)))
(assert (or (= x_21 x_18) (= x_21 x_16) (= x_21 literal_11) (= x_21 x_14)))
(assert (= x_22 (str.++ x_21 x_12)))
(assert (= literal_23 "\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x5c\x6e"))
(assert (or (= x_27 x_22) (= x_27 x_18) (= x_27 x_16) (= x_27 literal_11) (= x_27 x_14)))
(assert (= x_28 (str.++ literal_23 x_27)))
(assert (= literal_29 "\x5c\x6e\x3c\x2f\x74\x64\x3e\x3c\x2f\x74\x72\x3e\x5c\x6e\x3c\x2f\x74\x61\x62\x6c\x65\x3e\x5c\x6e"))
(assert (= x_31 (str.++ x_28 literal_29)))
(assert (= literal_30 "\x3c\x74\x72\x3e\x3c\x74\x64\x20\x61\x6c\x69\x67\x6e\x3d\x27\x63\x65\x6e\x74\x65\x72\x27\x3e\x5c\x6e"))
(assert (= x_32 (str.++ literal_30 sigmaStar_2)))
(assert (= x_33 (str.++ x_32 x_31)))
(assert (str.in.re x_33 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_33 "\x61" 0)) (str.len x_33)))
(check-sat)
(get-model)
