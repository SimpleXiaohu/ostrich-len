(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_6 () String)
(assert (= literal_6 "\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x22\x65\x76\x65\x6e\x74\x5f\x75\x73\x65\x72\x22\x20\x73\x74\x79\x6c\x65\x3d\x22\x77\x69\x64\x74\x68\x3a\x20\x38\x70\x78\x3b\x22\x3e\x3c\x2f\x74\x64\x3e\x3c\x74\x64\x3e\x3c\x73\x74\x72\x6f\x6e\x67\x3e\x75\x73\x65\x72\x65\x76\x65\x6e\x74\x73\x3a\x3c\x2f\x73\x74\x72\x6f\x6e\x67\x3e\x20"))
(assert (str.in.re literal_6 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_6  "\x61" 0)) (str.len literal_6 )))
(check-sat)
(get-model)
