(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun literal_14 () String)
(declare-fun x_15 () String)
(declare-fun literal_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_16 () String)
(declare-fun literal_17 () String)
(declare-fun x_18 () String)
(assert (= literal_14 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x5c\x22\x73\x75\x62\x6d\x69\x74\x5c\x22\x20\x76\x61\x6c\x75\x65\x3d\x5c\x22"))
(assert (= literal_12 "\x6d\x65\x73\x73\x61\x67\x65\x73"))
(assert (= literal_13 "\x6d\x65\x73\x73\x61\x67\x65\x73\x28\x6d\x65\x73\x73\x61\x67\x65\x29"))
(assert (or (= x_15 literal_12) (= x_15 literal_13)))
(assert (= x_16 (str.++ literal_14 x_15)))
(assert (= literal_17 "\x5c\x22\x20\x6f\x6e\x63\x6c\x69\x63\x6b\x3d\x5c\x22\x72\x65\x74\x75\x72\x6e\x20\x6f\x70\x65\x6e\x70\x6f\x70\x75\x70\x28\x27\x2f\x6d\x65\x73\x73\x61\x67\x65\x2f\x69\x6e\x64\x65\x78\x2e\x70\x68\x70\x27\x2c\x20\x27\x6d\x65\x73\x73\x61\x67\x65\x27\x2c\x20\x27\x6d\x65\x6e\x75\x62\x61\x72\x3d\x30\x2c\x6c\x6f\x63\x61\x74\x69\x6f\x6e\x3d\x30\x2c\x73\x63\x72\x6f\x6c\x6c\x62\x61\x72\x73\x2c\x73\x74\x61\x74\x75\x73\x2c\x72\x65\x73\x69\x7a\x61\x62\x6c\x65\x2c\x77\x69\x64\x74\x68\x3d\x34\x30\x30\x2c\x68\x65\x69\x67\x68\x74\x3d\x35\x30\x30\x27\x2c\x20\x30\x29\x3b\x5c\x22\x20\x2f\x3e"))
(assert (= x_18 (str.++ x_16 literal_17)))
(assert (str.in.re x_18 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_18 "\x61" 0)) (str.len x_18)))
(check-sat)
(get-model)
