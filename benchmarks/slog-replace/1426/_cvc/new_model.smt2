(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_6 () String)
(assert (= literal_6 "\x3c\x74\x72\x3e\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x22\x63\x30\x22\x3e\x61\x76\x61\x69\x6c\x61\x62\x6c\x65\x64\x61\x74\x65\x3a\x3c\x2f\x74\x64\x3e"))
(assert (str.in.re literal_6 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_6  "\x61" 0)) (str.len literal_6 )))
(check-sat)
(get-model)
