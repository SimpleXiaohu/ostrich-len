(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_4 () String)
(declare-fun sigmaStar_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_14 () String)
(declare-fun literal_16 () String)
(declare-fun x_15 () String)
(declare-fun x_17 () String)
(declare-fun literal_18 () String)
(declare-fun x_19 () String)
(declare-fun x_20 () String)
(declare-fun literal_21 () String)
(declare-fun x_22 () String)
(declare-fun x_23 () String)
(declare-fun x_24 () String)
(declare-fun literal_25 () String)
(declare-fun x_26 () String)
(declare-fun x_27 () String)
(declare-fun literal_28 () String)
(declare-fun x_29 () String)
(assert (= literal_13 "\x3f\x61\x69\x64\x3d"))
(assert (= x_14 (str.++ literal_13 sigmaStar_12)))
(assert (= literal_16 "\x3c\x61\x20\x68\x72\x65\x66\x3d\x27\x70\x61\x6e\x65\x6c\x73\x2e\x70\x68\x70"))
(assert (or (= x_15 sigmaStar_3) (= x_15 x_14)))
(assert (= x_17 (str.++ literal_16 x_15)))
(assert (= literal_18 "\x27\x3e"))
(assert (= x_19 (str.++ x_17 literal_18)))
(assert (= x_20 (str.++ x_19 sigmaStar_4)))
(assert (= literal_21 "\x3c\x2f\x61\x3e\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x0d\x0a\x3c\x61\x20\x68\x72\x65\x66\x3d\x27\x69\x6e\x64\x65\x78\x2e\x70\x68\x70"))
(assert (= x_22 (str.++ x_20 literal_21)))
(assert (or (= x_23 sigmaStar_1) (= x_23 x_14)))
(assert (= x_24 (str.++ x_22 x_23)))
(assert (= literal_25 "\x27\x3e"))
(assert (= x_26 (str.++ x_24 literal_25)))
(assert (= x_27 (str.++ x_26 sigmaStar_2)))
(assert (= literal_28 "\x3c\x2f\x61\x3e\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x0d\x0a\x3c\x2f\x63\x65\x6e\x74\x65\x72\x3e\x5c\x6e"))
(assert (= x_29 (str.++ x_27 literal_28)))
(assert (str.in.re x_29 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_29 "\x61" 0)) (str.len x_29)))
(check-sat)
(get-model)
