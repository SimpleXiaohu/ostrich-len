(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_9 () String)
(declare-fun x_8 () String)
(declare-fun literal_6 () String)
(declare-fun literal_7 () String)
(declare-fun x_10 () String)
(declare-fun literal_11 () String)
(declare-fun x_12 () String)
(declare-fun literal_15 () String)
(declare-fun x_16 () String)
(declare-fun literal_14 () String)
(declare-fun x_17 () String)
(declare-fun literal_20 () String)
(declare-fun x_21 () String)
(declare-fun literal_19 () String)
(declare-fun x_22 () String)
(declare-fun literal_23 () String)
(declare-fun x_24 () String)
(assert (= literal_9 "\x20\x3c\x61\x20\x68\x72\x65\x66\x3d\x22"))
(assert (= literal_6 "\x2f\x63\x6f\x75\x72\x73\x65\x2f\x6d\x6f\x64\x2e\x70\x68\x70\x3f\x75\x70\x64\x61\x74\x65\x3d\x26\x61\x6d\x70\x3b\x72\x65\x74\x75\x72\x6e\x3d\x74\x72\x75\x65\x26\x61\x6d\x70\x3b\x73\x65\x73\x73\x6b\x65\x79\x3d"))
(assert (= literal_7 "\x2f\x63\x61\x6c\x65\x6e\x64\x61\x72\x2f\x65\x76\x65\x6e\x74\x2e\x70\x68\x70\x3f\x61\x63\x74\x69\x6f\x6e\x3d\x65\x64\x69\x74\x26\x61\x6d\x70\x3b\x69\x64\x3d"))
(assert (or (= x_8 literal_6) (= x_8 literal_7)))
(assert (= x_10 (str.++ literal_9 x_8)))
(assert (= literal_11 "\x22\x3e\x3c\x69\x6d\x67\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x73\x72\x63\x3d\x22"))
(assert (= x_12 (str.++ x_10 literal_11)))
(assert (= literal_15 "\x2f\x74\x2f\x65\x64\x69\x74\x2e\x67\x69\x66\x22\x20\x61\x6c\x74\x3d\x22"))
(assert (= x_16 (str.++ x_12 literal_15)))
(assert (= literal_14 "\x74\x74\x5f\x65\x64\x69\x74\x65\x76\x65\x6e\x74"))
(assert (= x_17 (str.++ x_16 literal_14)))
(assert (= literal_20 "\x22\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x74\x69\x74\x6c\x65\x3d\x22"))
(assert (= x_21 (str.++ x_17 literal_20)))
(assert (= literal_19 "\x74\x74\x5f\x65\x64\x69\x74\x65\x76\x65\x6e\x74"))
(assert (= x_22 (str.++ x_21 literal_19)))
(assert (= literal_23 "\x22\x20\x2f\x3e\x3c\x2f\x61\x3e"))
(assert (= x_24 (str.++ x_22 literal_23)))
(assert (str.in.re x_24 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_24 "\x61" 0)) (str.len x_24)))
(check-sat)
(get-model)
