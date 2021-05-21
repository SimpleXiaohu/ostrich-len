(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_8 () String)
(assert (= literal_8 "\x3c\x70\x20\x61\x6c\x69\x67\x6e\x3d\x5c\x22\x63\x65\x6e\x74\x65\x72\x5c\x22\x3e\x3c\x61\x20\x68\x72\x65\x66\x3d\x5c\x22\x61\x73\x73\x65\x73\x73\x6d\x65\x6e\x74\x73\x2e\x70\x68\x70\x3f\x69\x64\x3d\x26\x61\x6d\x70\x3b\x61\x63\x74\x69\x6f\x6e\x3d\x72\x65\x67\x72\x61\x64\x65\x73\x74\x75\x64\x65\x6e\x74\x61\x73\x73\x65\x73\x73\x6d\x65\x6e\x74\x73\x5c\x22\x3e\x72\x65\x67\x72\x61\x64\x65\x73\x74\x75\x64\x65\x6e\x74\x61\x73\x73\x65\x73\x73\x6d\x65\x6e\x74\x73\x3c\x2f\x61\x3e\x20"))
(assert (str.in.re literal_8 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_8  "\x61" 0)) (str.len literal_8 )))
(check-sat)
(get-model)
