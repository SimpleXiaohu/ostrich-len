(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_12 () String)
(assert (= literal_12 "\x20\x7c\x20\x28\x3c\x61\x20\x68\x72\x65\x66\x3d\x5c\x22\x72\x65\x70\x6f\x73\x69\x74\x6f\x72\x79\x5f\x64\x65\x70\x6c\x6f\x79\x2e\x70\x68\x70\x3f\x66\x69\x6c\x65\x3d\x64\x69\x72\x65\x63\x74\x6f\x72\x79\x26\x61\x6c\x6c\x3d\x66\x6f\x72\x63\x65\x5c\x22\x3e\x64\x65\x70\x6c\x6f\x79\x61\x6c\x6c\x3c\x2f\x61\x3e\x29\x20"))
(assert (str.in.re literal_12 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_12  "\x61" 0)) (str.len literal_12 )))
(check-sat)
(get-model)
