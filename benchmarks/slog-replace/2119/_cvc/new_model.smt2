(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_8 () String)
(assert (= literal_8 "\x3c\x64\x69\x76\x20\x63\x6c\x61\x73\x73\x3d\x22\x68\x65\x61\x64\x65\x72\x22\x3e\x64\x65\x6c\x65\x74\x65\x65\x76\x65\x6e\x74\x3a\x20\x3c\x2f\x64\x69\x76\x3e"))
(assert (str.in.re literal_8 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_8  "\x61" 0)) (str.len literal_8 )))
(check-sat)
(get-model)