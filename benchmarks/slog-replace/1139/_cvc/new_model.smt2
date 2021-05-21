(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_7 () String)
(assert (= literal_7 "\x3c\x62\x72\x20\x2f\x3e\x3c\x62\x3e\x6d\x61\x78\x69\x6d\x75\x6d\x67\x72\x61\x64\x65\x3c\x2f\x62\x3e\x3a\x20\x20\x20"))
(assert (str.in.re literal_7 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_7  "\x61" 0)) (str.len literal_7 )))
(check-sat)
(get-model)
