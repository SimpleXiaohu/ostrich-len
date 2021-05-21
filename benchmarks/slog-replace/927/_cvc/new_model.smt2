(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_4 () String)
(assert (= literal_4 "\x3c\x63\x65\x6e\x74\x65\x72\x3e\x3c\x62\x72\x20\x2f\x3e\x61\x72\x65\x79\x6f\x75\x73\x75\x72\x65\x64\x65\x6c\x65\x74\x65\x63\x6f\x6d\x6d\x65\x6e\x74"))
(assert (str.in.re literal_4 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_4  "\x61" 0)) (str.len literal_4 )))
(check-sat)
(get-model)
