(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_6 () String)
(assert (= literal_6 "\x3c\x64\x69\x76\x20\x63\x6c\x61\x73\x73\x3d\x22\x65\x64\x69\x74\x65\x6e\x64\x22\x3e\x3c\x73\x74\x72\x6f\x6e\x67\x3e\x65\x64\x69\x74\x69\x6e\x67\x65\x6e\x64\x73\x3a\x3c\x2f\x73\x74\x72\x6f\x6e\x67\x3e\x20"))
(assert (str.in.re literal_6 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_6  "\x61" 0)) (str.len literal_6 )))
(check-sat)
(get-model)
