(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_11 () String)
(assert (= literal_11 "\x3c\x61\x20\x68\x72\x65\x66\x3d\x5c\x22\x72\x65\x70\x6f\x72\x74\x2e\x70\x68\x70\x3f\x61\x63\x74\x69\x6f\x6e\x3d\x73\x75\x6d\x6d\x61\x72\x79\x26\x61\x6d\x70\x3b\x69\x64\x3d\x69\x64\x5c\x22\x3e\x73\x75\x6d\x6d\x61\x72\x79\x3c\x2f\x61\x3e"))
(assert (str.in.re literal_11 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_11  "\x61" 0)) (str.len literal_11 )))
(check-sat)
(get-model)
