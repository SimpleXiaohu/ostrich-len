(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_6 () String)
(assert (= literal_6 "\x3c\x6f\x70\x74\x69\x6f\x6e\x20\x76\x61\x6c\x75\x65\x3d\x22\x23\x23\x65\x64\x69\x74\x23\x23\x22\x3e\x23\x23\x65\x64\x69\x74\x23\x23\x3c\x2f\x6f\x70\x74\x69\x6f\x6e\x3e"))
(assert (str.in.re literal_6 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_6  "\x61" 0)) (str.len literal_6 )))
(check-sat)
(get-model)
