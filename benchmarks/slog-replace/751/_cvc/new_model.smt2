(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_1 () String)
(assert (= literal_1 "\x59\x6f\x75\x72\x20\x73\x65\x74\x74\x69\x6e\x67\x73\x20\x68\x61\x76\x65\x20\x62\x65\x65\x6e\x20\x73\x61\x76\x65\x64\x2e\x20\x59\x6f\x75\x20\x63\x61\x6e\x20\x6e\x6f\x77\x20\x74\x72\x79"))
(assert (str.in.re literal_1 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_1  "\x61" 0)) (str.len literal_1 )))
(check-sat)
(get-model)
