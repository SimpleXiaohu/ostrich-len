(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_11 () String)
(declare-fun sigmaStar_14 () String)
(declare-fun sigmaStar_15 () String)
(declare-fun sigmaStar_17 () String)
(declare-fun sigmaStar_19 () String)
(declare-fun sigmaStar_21 () String)
(declare-fun sigmaStar_25 () String)
(declare-fun sigmaStar_26 () String)
(declare-fun literal_28 () String)
(declare-fun x_29 () String)
(declare-fun literal_30 () String)
(declare-fun x_31 () String)
(declare-fun literal_32 () String)
(declare-fun x_33 () String)
(declare-fun literal_34 () String)
(declare-fun x_35 () String)
(declare-fun literal_38 () String)
(declare-fun x_39 () String)
(declare-fun literal_37 () String)
(declare-fun x_40 () String)
(declare-fun literal_41 () String)
(declare-fun x_42 () String)
(assert (= literal_28 "\x3f\x3d\x26\x3d\x26\x3d\x26\x3d\x26\x3d"))
(assert (= x_29 (str.++ literal_28 sigmaStar_26)))
(assert (= literal_30 "\x26"))
(assert (= x_31 (str.++ x_29 literal_30)))
(assert (= literal_32 "\x3d"))
(assert (= x_33 (str.++ x_31 literal_32)))
(assert (= literal_34 "\x3c\x61\x20\x74\x61\x72\x67\x65\x74\x3d\x22\x4c\x41\x4d\x53\x20\x4d\x6f\x6e\x69\x74\x6f\x72\x22\x20\x74\x69\x74\x6c\x65\x3d\x22\x4c\x41\x4d\x53\x20\x4d\x6f\x6e\x69\x74\x6f\x72\x22\x20\x68\x72\x65\x66\x3d\x22"))
(assert (= x_35 (str.++ literal_34 x_33)))
(assert (= literal_38 "\x22\x3e"))
(assert (= x_39 (str.++ x_35 literal_38)))
(assert (= literal_37 "\x6f\x70\x65\x6e\x6d\x6f\x6e\x69\x74\x6f\x72"))
(assert (= x_40 (str.++ x_39 literal_37)))
(assert (= literal_41 "\x3c\x2f\x61\x3e"))
(assert (= x_42 (str.++ x_40 literal_41)))
(assert (str.in.re x_42 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_42 "\x61" 0)) (str.len x_42)))
(check-sat)
(get-model)
