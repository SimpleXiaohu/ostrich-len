(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun literal_12 () String)
(assert (= literal_12 "\x3c\x70\x20\x61\x6c\x69\x67\x6e\x3d\x5c\x22\x63\x65\x6e\x74\x65\x72\x5c\x22\x3e\x6e\x65\x78\x74\x73\x65\x73\x73\x69\x6f\x6e\x3a\x20\x20\x28\x29\x3c\x2f\x70\x3e"))
(assert (str.in.re literal_12 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_12  "\x61" 0)) (str.len literal_12 )))
(check-sat)
(get-model)
