(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_5 () String)
(assert (= literal_5 "\x73\x69\x74\x65\x3a\x20\x3c\x73\x74\x72\x6f\x6e\x67\x3e\x3c\x2f\x73\x74\x72\x6f\x6e\x67\x3e\x3c\x62\x72\x20\x2f\x3e"))
(assert (str.in.re literal_5 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_5  "\x61" 0)) (str.len literal_5 )))
(check-sat)
(get-model)