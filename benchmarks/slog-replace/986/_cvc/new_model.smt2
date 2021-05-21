(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_4 () String)
(declare-fun sigmaStar_5 () String)
(declare-fun sigmaStar_6 () String)
(declare-fun sigmaStar_7 () String)
(declare-fun sigmaStar_8 () String)
(declare-fun literal_26 () String)
(declare-fun x_25 () String)
(declare-fun literal_16 () String)
(declare-fun literal_24 () String)
(declare-fun literal_14 () String)
(declare-fun x_29 () String)
(declare-fun literal_30 () String)
(declare-fun x_31 () String)
(declare-fun literal_34 () String)
(declare-fun x_35 () String)
(declare-fun literal_33 () String)
(declare-fun x_36 () String)
(declare-fun literal_37 () String)
(declare-fun x_38 () String)
(declare-fun sigmaStar_39 () String)
(declare-fun literal_40 () String)
(declare-fun x_41 () String)
(assert (= literal_26 "\x20\x41\x4e\x44\x20\x28\x63\x2e\x67\x72\x6f\x75\x70\x69\x64\x3d\x27"))
(assert (= literal_16 "\x63\x6f\x75\x72\x73\x65"))
(assert (= literal_24 "\x30"))
(assert (= literal_14 "\x63\x6f\x75\x72\x73\x65"))
(assert (or (= x_25 literal_16) (= x_25 literal_24) (= x_25 literal_14)))
(assert (= x_29 (str.++ literal_26 x_25)))
(assert (= literal_30 "\x20\x4f\x52\x20\x63\x2e\x67\x72\x6f\x75\x70\x69\x64\x3d\x30\x27\x29"))
(assert (= x_31 (str.++ x_29 literal_30)))
(assert (= literal_34 "\x53\x45\x4c\x45\x43\x54\x20\x44\x49\x53\x54\x49\x4e\x43\x54\x20\x75\x2e\x69\x64\x2c\x20\x75\x2e\x66\x69\x72\x73\x74\x6e\x61\x6d\x65\x2c\x20\x75\x2e\x6c\x61\x73\x74\x6e\x61\x6d\x65\x2c\x20\x75\x2e\x70\x69\x63\x74\x75\x72\x65\x2c\x20\x63\x2e\x6c\x61\x73\x74\x6d\x65\x73\x73\x61\x67\x65\x70\x69\x6e\x67\x2c\x20\x63\x2e\x66\x69\x72\x73\x74\x70\x69\x6e\x67\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x46\x52\x4f\x4d\x20\x63\x68\x61\x74\x5f\x75\x73\x65\x72\x73\x20\x63\x2c\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x75\x73\x65\x72\x20\x75\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x57\x48\x45\x52\x45\x20\x63\x2e\x63\x68\x61\x74\x69\x64\x20\x3d\x20\x27\x27\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x41\x4e\x44\x20\x75\x2e\x69\x64\x20\x3d\x20\x63\x2e\x75\x73\x65\x72\x69\x64\x20"))
(assert (= literal_33 ""))
(assert (or (= x_35 x_31) (= x_35 literal_33)))
(assert (= x_36 (str.++ literal_34 x_35)))
(assert (= literal_37 "\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x4f\x52\x44\x45\x52\x20\x42\x59\x20\x63\x2e\x66\x69\x72\x73\x74\x70\x69\x6e\x67\x20\x41\x53\x43"))
(assert (= x_38 (str.++ x_36 literal_37)))
(assert (= literal_40 "\x3c\x62\x72\x20\x2f\x3e"))
(assert (= x_41 (str.++ x_38 literal_40)))
(assert (str.in.re x_41 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_41 "\x61" 0)) (str.len x_41)))
(check-sat)
(get-model)
