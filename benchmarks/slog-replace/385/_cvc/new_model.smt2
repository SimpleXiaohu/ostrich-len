(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_12 () String)
(assert (= literal_12 "\x3c\x70\x20\x61\x6c\x69\x67\x6e\x3d\x22\x63\x65\x6e\x74\x65\x72\x22\x3e\x6f\x72\x3c\x62\x72\x20\x2f\x3e\x3c\x62\x72\x20\x2f\x3e\x63\x6f\x6e\x74\x69\x6e\x75\x65\x74\x6f\x61\x6e\x73\x77\x65\x72\x3c\x2f\x70\x3e\x5c\x6e"))
(assert (str.in.re literal_12 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_12  "\x61" 0)) (str.len literal_12 )))
(check-sat)
(get-model)