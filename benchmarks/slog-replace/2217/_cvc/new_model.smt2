(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_4 () String)
(assert (= literal_4 "\x61\x64\x64\x63\x61\x74\x65\x67\x6f\x72\x79\x3a\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x74\x65\x78\x74\x22\x20\x6e\x61\x6d\x65\x3d\x22\x6e\x61\x6d\x65\x22\x20\x73\x69\x7a\x65\x3d\x22\x32\x30\x22\x20\x2f\x3e"))
(assert (str.in.re literal_4 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_4  "\x61" 0)) (str.len literal_4 )))
(check-sat)
(get-model)
