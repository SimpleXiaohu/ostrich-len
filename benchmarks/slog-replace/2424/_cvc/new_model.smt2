(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_7 () String)
(assert (= literal_7 "\x20\x20\x20\x68\x72\x65\x66\x3d\x5c\x22\x2e\x2e\x2f\x6d\x6f\x64\x2f\x2f\x76\x69\x65\x77\x2e\x70\x68\x70\x3f\x69\x64\x3d\x5c\x22\x3e\x3c\x2f\x61\x3e\x3c\x2f\x74\x64\x3e"))
(assert (str.in.re literal_7 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_7  "\x61" 0)) (str.len literal_7 )))
(check-sat)
(get-model)