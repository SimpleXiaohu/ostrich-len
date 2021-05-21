(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun literal_7 () String)
(assert (= literal_7 "\x3c\x62\x3e\x2f\x6c\x61\x6e\x67\x2f\x65\x6e\x5f\x75\x74\x66\x38\x3c\x2f\x62\x3e\x20\x26\x6e\x62\x73\x70\x3b\x20"))
(assert (str.in.re literal_7 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_7  "\x61" 0)) (str.len literal_7 )))
(check-sat)
(get-model)
