(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_3 () String)
(assert (= literal_3 "\x3c\x64\x69\x76\x20\x61\x6c\x69\x67\x6e\x3d\x22\x63\x65\x6e\x74\x65\x72\x22\x3e\x3c\x2f\x64\x69\x76\x3e\x5c\x6e"))
(assert (str.in.re literal_3 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_3  "\x61" 0)) (str.len literal_3 )))
(check-sat)
(get-model)
