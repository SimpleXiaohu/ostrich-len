(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_10 () String)
(declare-fun literal_14 () String)
(declare-fun x_13 () String)
(declare-fun literal_11 () String)
(declare-fun literal_12 () String)
(declare-fun x_16 () String)
(declare-fun literal_15 () String)
(declare-fun x_17 () String)
(declare-fun literal_18 () String)
(declare-fun x_19 () String)
(declare-fun x_20 () String)
(declare-fun x_21 () String)
(declare-fun literal_22 () String)
(declare-fun x_23 () String)
(declare-fun x_24 () String)
(declare-fun literal_25 () String)
(declare-fun x_26 () String)
(assert (= literal_14 "\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27"))
(assert (= literal_11 "\x74\x62\x6c\x32"))
(assert (= literal_12 "\x74\x62\x6c\x31"))
(assert (or (= x_13 literal_11) (= x_13 literal_12)))
(assert (= x_16 (str.++ literal_14 x_13)))
(assert (= literal_15 "\x3f\x61\x69\x64\x3d"))
(assert (= x_17 (str.++ literal_15 sigmaStar_10)))
(assert (= literal_18 "\x27\x20\x73\x74\x79\x6c\x65\x3d\x27\x70\x61\x64\x64\x69\x6e\x67\x2d\x6c\x65\x66\x74\x3a\x31\x30\x70\x78\x3b\x70\x61\x64\x64\x69\x6e\x67\x2d\x72\x69\x67\x68\x74\x3a\x31\x30\x70\x78\x3b\x27\x3e\x3c\x73\x70\x61\x6e\x20\x63\x6c\x61\x73\x73\x3d\x27\x73\x6d\x61\x6c\x6c\x27\x3e\x3c\x61\x20\x68\x72\x65\x66\x3d\x27\x73\x65\x74\x74\x69\x6e\x67\x73\x5f\x74\x69\x6d\x65\x2e\x70\x68\x70"))
(assert (= x_19 (str.++ x_16 literal_18)))
(assert (or (= x_20 sigmaStar_1) (= x_20 x_17)))
(assert (= x_21 (str.++ x_19 x_20)))
(assert (= literal_22 "\x27\x3e"))
(assert (= x_23 (str.++ x_21 literal_22)))
(assert (= x_24 (str.++ x_23 sigmaStar_0)))
(assert (= literal_25 "\x3c\x2f\x61\x3e\x3c\x2f\x73\x70\x61\x6e\x3e\x3c\x2f\x74\x64\x3e\x5c\x6e"))
(assert (= x_26 (str.++ x_24 literal_25)))
(assert (str.in.re x_26 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_26 "\x61" 0)) (str.len x_26)))
(check-sat)
(get-model)
