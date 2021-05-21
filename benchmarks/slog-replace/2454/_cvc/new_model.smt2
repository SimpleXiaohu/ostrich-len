(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_7 () String)
(declare-fun x_8 () String)
(declare-fun literal_2 () String)
(declare-fun literal_5 () String)
(declare-fun x_11 () String)
(declare-fun literal_14 () String)
(declare-fun x_15 () String)
(declare-fun literal_16 () String)
(declare-fun x_17 () String)
(assert (= literal_7 "\x20\x73\x72\x63\x3d\x5c\x22"))
(assert (= literal_2 "\x2f\x70\x69\x78"))
(assert (= literal_5 "\x2f\x74\x68\x65\x6d\x65\x2f\x2f\x70\x69\x78"))
(assert (or (= x_8 literal_2) (= x_8 literal_5)))
(assert (= x_11 (str.++ literal_7 x_8)))
(assert (= literal_14 "\x2f\x74\x2f\x72\x65\x73\x74\x6f\x72\x65\x2e\x67\x69\x66\x5c\x22\x20\x68\x65\x69\x67\x68\x74\x3d\x5c\x22\x31\x31\x5c\x22\x20\x77\x69\x64\x74\x68\x3d\x5c\x22\x31\x31\x5c\x22\x20\x62\x6f\x72\x64\x65\x72\x3d\x5c\x22\x30\x5c\x22\x3e\x3c\x2f\x61\x3e\x20"))
(assert (= x_15 (str.++ x_11 literal_14)))
(assert (= literal_16 "\x3c\x61\x20\x74\x69\x74\x6c\x65\x3d\x22\x72\x65\x73\x74\x6f\x72\x65\x5c\x22\x20\x68\x72\x65\x66\x3d\x5c\x22\x2e\x2e\x2f\x66\x69\x6c\x65\x73\x2f\x69\x6e\x64\x65\x78\x2e\x70\x68\x70\x3f\x69\x64\x3d\x26\x77\x64\x69\x72\x3d\x2f\x62\x61\x63\x6b\x75\x70\x64\x61\x74\x61\x5c\x22\x3e\x3c\x69\x6d\x67"))
(assert (= x_17 (str.++ literal_16 x_15)))
(assert (str.in.re x_17 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_17 "\x61" 0)) (str.len x_17)))
(check-sat)
(get-model)
