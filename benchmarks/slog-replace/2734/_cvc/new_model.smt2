(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_2 () String)
(assert (= literal_2 "\x73\x74\x69\x63\x6b\x79\x62\x6c\x6f\x63\x6b\x73\x64\x75\x70\x6c\x69\x63\x61\x74\x65\x6e\x6f\x74\x69\x63\x65"))
(assert (str.in.re literal_2 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_2  "\x61" 0)) (str.len literal_2 )))
(check-sat)
(get-model)
