(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun literal_6 () String)
(declare-fun x_8 () String)
(declare-fun literal_9 () String)
(declare-fun x_10 () String)
(declare-fun literal_7 () String)
(declare-fun x_11 () String)
(declare-fun literal_12 () String)
(declare-fun x_13 () String)
(declare-fun x_14 () String)
(declare-fun literal_15 () String)
(declare-fun x_16 () String)
(declare-fun literal_17 () String)
(declare-fun x_18 () String)
(declare-fun literal_19 () String)
(declare-fun x_20 () String)
(declare-fun literal_21 () String)
(declare-fun x_23 () String)
(declare-fun literal_24 () String)
(declare-fun x_25 () String)
(declare-fun literal_22 () String)
(declare-fun x_26 () String)
(declare-fun literal_27 () String)
(declare-fun x_28 () String)
(declare-fun literal_29 () String)
(declare-fun x_30 () String)
(assert (= literal_6 "\x3c\x66\x6f\x72\x6d\x20\x6d\x65\x74\x68\x6f\x64\x3d\x22\x70\x6f\x73\x74\x22\x20\x65\x6e\x63\x74\x79\x70\x65\x3d\x22\x6d\x75\x6c\x74\x69\x70\x61\x72\x74\x2f\x66\x6f\x72\x6d\x2d\x64\x61\x74\x61\x22\x20\x61\x63\x74\x69\x6f\x6e\x3d\x22\x2f\x63\x6f\x75\x72\x73\x65\x2f\x69\x6d\x70\x6f\x72\x74\x2f\x67\x72\x6f\x75\x70\x73\x2f\x69\x6e\x64\x65\x78\x2e\x70\x68\x70\x3f\x69\x64\x3d"))
(assert (= x_8 (str.++ literal_6 sigmaStar_0)))
(assert (= literal_9 "\x22\x3e"))
(assert (= x_10 (str.++ x_8 literal_9)))
(assert (= literal_7 "\x63\x68\x6f\x6f\x73\x65"))
(assert (= x_11 (str.++ x_10 literal_7)))
(assert (= literal_12 "\x3a\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x68\x69\x64\x64\x65\x6e\x22\x20\x6e\x61\x6d\x65\x3d\x22\x4d\x41\x58\x5f\x46\x49\x4c\x45\x5f\x53\x49\x5a\x45\x22\x20\x76\x61\x6c\x75\x65\x3d\x22"))
(assert (= x_13 (str.++ x_11 literal_12)))
(assert (= x_14 (str.++ x_13 sigmaStar_2)))
(assert (= literal_15 "\x22\x3e"))
(assert (= x_16 (str.++ x_14 literal_15)))
(assert (= literal_17 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x68\x69\x64\x64\x65\x6e\x22\x20\x6e\x61\x6d\x65\x3d\x22\x73\x65\x73\x73\x6b\x65\x79\x22\x20\x76\x61\x6c\x75\x65\x3d\x22"))
(assert (= x_18 (str.++ x_16 literal_17)))
(assert (= literal_19 "\x22\x3e"))
(assert (= x_20 (str.++ x_18 literal_19)))
(assert (= literal_21 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x66\x69\x6c\x65\x22\x20\x6e\x61\x6d\x65\x3d\x22\x75\x73\x65\x72\x66\x69\x6c\x65\x22\x20\x73\x69\x7a\x65\x3d\x22\x33\x30\x22\x3e"))
(assert (= x_23 (str.++ x_20 literal_21)))
(assert (= literal_24 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x73\x75\x62\x6d\x69\x74\x22\x20\x76\x61\x6c\x75\x65\x3d\x22"))
(assert (= x_25 (str.++ x_23 literal_24)))
(assert (= literal_22 "\x69\x6d\x70\x6f\x72\x74\x67\x72\x6f\x75\x70\x73"))
(assert (= x_26 (str.++ x_25 literal_22)))
(assert (= literal_27 "\x22\x3e"))
(assert (= x_28 (str.++ x_26 literal_27)))
(assert (= literal_29 "\x3c\x2f\x66\x6f\x72\x6d\x3e\x3c\x2f\x62\x72\x3e"))
(assert (= x_30 (str.++ x_28 literal_29)))
(assert (str.in.re x_30 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_30 "\x61" 0)) (str.len x_30)))
(check-sat)
(get-model)
