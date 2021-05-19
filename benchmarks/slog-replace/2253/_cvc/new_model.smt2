(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_6 () String)
(assert (= literal_6 "\x3c\x73\x74\x72\x6f\x6e\x67\x3e\x65\x6e\x74\x72\x79\x73\x61\x76\x65\x64\x3c\x2f\x73\x74\x72\x6f\x6e\x67\x3e\x3c\x62\x72\x20\x2f\x3e"))
(assert (str.in.re literal_6 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_6  "\x61" 0)) (str.len literal_6 )))
(check-sat)
(get-model)
