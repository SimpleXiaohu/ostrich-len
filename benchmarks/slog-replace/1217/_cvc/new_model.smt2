(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_12 () String)
(assert (= literal_12 "\x20\x20\x3c\x74\x64\x20\x61\x6c\x69\x67\x6e\x3d\x22\x72\x69\x67\x68\x74\x22\x3e\x3c\x70\x3e\x3c\x62\x3e\x67\x65\x6e\x65\x72\x61\x6c\x63\x6f\x6d\x6d\x65\x6e\x74\x2f\x3c\x62\x72\x20\x2f\x3e\x72\x65\x61\x73\x6f\x6e\x66\x6f\x72\x61\x64\x6a\x75\x73\x74\x6d\x65\x6e\x74\x3a\x3c\x2f\x62\x3e\x3c\x2f\x70\x3e\x3c\x2f\x74\x64\x3e\x5c\x6e"))
(assert (str.in.re literal_12 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_12  "\x61" 0)) (str.len literal_12 )))
(check-sat)
(get-model)
