(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun literal_4 () String)
(declare-fun x_6 () String)
(declare-fun literal_8 () String)
(declare-fun x_9 () String)
(declare-fun literal_7 () String)
(declare-fun x_10 () String)
(declare-fun literal_13 () String)
(declare-fun x_15 () String)
(declare-fun x_14 () String)
(declare-fun literal_11 () String)
(declare-fun literal_12 () String)
(declare-fun x_16 () String)
(declare-fun literal_17 () String)
(declare-fun x_18 () String)
(declare-fun x_19 () String)
(declare-fun literal_20 () String)
(declare-fun x_21 () String)
(assert (= literal_4 "\x53\x45\x4c\x45\x43\x54\x20\x2a\x20\x46\x52\x4f\x4d\x20"))
(assert (= x_6 (str.++ literal_4 sigmaStar_0)))
(assert (= literal_8 "\x73\x69\x74\x65\x5f\x6c\x69\x6e\x6b\x73\x20\x57\x48\x45\x52\x45\x20\x6c\x69\x6e\x6b\x5f\x70\x6f\x73\x69\x74\x69\x6f\x6e\x3c\x3d\x27\x32\x27\x20\x4f\x52\x44\x45\x52\x20\x42\x59\x20\x6c\x69\x6e\x6b\x5f\x6f\x72\x64\x65\x72"))
(assert (= x_9 (str.++ x_6 literal_8)))
(assert (= literal_7 "\x3c\x69\x6d\x67\x20\x73\x72\x63\x3d\x27\x69\x6d\x61\x67\x65\x73\x2f\x62\x75\x6c\x6c\x65\x74\x2e\x67\x69\x66\x27\x20\x61\x6c\x74\x3d\x27\x27\x3e\x20\x3c\x61\x20\x68\x72\x65\x66\x3d\x27"))
(assert (= x_10 (str.++ literal_7 x_9)))
(assert (= literal_13 "\x27"))
(assert (= x_15 (str.++ x_10 literal_13)))
(assert (= literal_11 "\x20\x74\x61\x72\x67\x65\x74\x3d\x27\x5f\x62\x6c\x61\x6e\x6b\x27"))
(assert (= literal_12 ""))
(assert (or (= x_14 literal_11) (= x_14 literal_12)))
(assert (= x_16 (str.++ x_15 x_14)))
(assert (= literal_17 "\x20\x63\x6c\x61\x73\x73\x3d\x27\x73\x69\x64\x65\x27\x3e"))
(assert (= x_18 (str.++ x_16 literal_17)))
(assert (= x_19 (str.++ x_18 x_9)))
(assert (= literal_20 "\x3c\x2f\x61\x3e\x3c\x62\x72\x3e\x5c\x6e"))
(assert (= x_21 (str.++ x_19 literal_20)))
(assert (str.in.re x_21 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_21 "\x61" 0)) (str.len x_21)))
(check-sat)
(get-model)
