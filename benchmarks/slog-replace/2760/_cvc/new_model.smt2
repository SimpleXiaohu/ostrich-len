(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_6 () String)
(assert (= literal_6 "\x65\x6e\x72\x6f\x6c\x6d\x65\x6e\x74\x73\x3a\x20\x75\x73\x65\x72\x5f\x73\x74\x75\x64\x65\x6e\x74\x73"))
(assert (str.in.re literal_6 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_6  "\x61" 0)) (str.len literal_6 )))
(check-sat)
(get-model)
