(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_10 () String)
(assert (= literal_10 "\x3c\x70\x20\x61\x6c\x69\x67\x6e\x3d\x5c\x22\x72\x69\x67\x68\x74\x5c\x22\x3e\x3c\x61\x20\x68\x72\x65\x66\x3d\x5c\x22\x61\x73\x73\x65\x73\x73\x6d\x65\x6e\x74\x73\x2e\x70\x68\x70\x3f\x61\x63\x74\x69\x6f\x6e\x3d\x65\x64\x69\x74\x63\x6f\x6d\x6d\x65\x6e\x74\x26\x61\x6d\x70\x3b\x69\x64\x3d\x26\x61\x6d\x70\x3b\x63\x69\x64\x3d\x5c\x22\x3e\x65\x64\x69\x74\x3c\x2f\x61\x3e\x5c\x6e"))
(assert (str.in.re literal_10 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_10  "\x61" 0)) (str.len literal_10 )))
(check-sat)
(get-model)
