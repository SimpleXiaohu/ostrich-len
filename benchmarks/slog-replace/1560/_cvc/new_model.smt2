(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_10 () String)
(assert (= literal_10 "\x3c\x74\x68\x3e\x3c\x61\x20\x68\x72\x65\x66\x3d\x5c\x22\x72\x65\x70\x6f\x72\x74\x2e\x70\x68\x70\x3f\x69\x64\x3d\x69\x64\x26\x61\x6d\x70\x3b\x73\x6f\x72\x74\x3d\x66\x69\x72\x73\x74\x6e\x61\x6d\x65\x5c\x22\x3e\x6e\x61\x6d\x65\x3c\x2f\x61\x3e"))
(assert (str.in.re literal_10 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_10  "\x61" 0)) (str.len literal_10 )))
(check-sat)
(get-model)
