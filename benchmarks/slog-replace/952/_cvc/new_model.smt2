(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_2 () String)
(assert (= literal_2 "\x69\x6d\x70\x6f\x72\x74\x65\x64\x65\x6e\x74\x72\x69\x65\x73"))
(assert (str.in.re literal_2 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_2  "\x61" 0)) (str.len literal_2 )))
(check-sat)
(get-model)
