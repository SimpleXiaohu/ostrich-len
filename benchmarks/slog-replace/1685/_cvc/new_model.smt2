(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_10 () String)
(assert (= literal_10 "\x3c\x70\x20\x61\x6c\x69\x67\x6e\x3d\x22\x63\x65\x6e\x74\x65\x72\x22\x3e\x67\x72\x61\x64\x65\x6d\x65\x74\x68\x6f\x64\x3a\x20\x3c\x2f\x70\x3e"))
(assert (str.in.re literal_10 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_10  "\x61" 0)) (str.len literal_10 )))
(check-sat)
(get-model)
