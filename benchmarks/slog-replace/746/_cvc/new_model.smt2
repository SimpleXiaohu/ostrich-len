(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun literal_0 () String)
(assert (= literal_0 "\x2f\x62\x61\x63\x6b\x75\x70\x2f\x62\x62\x2f\x78\x73\x6c\x5f\x65\x6d\x75\x6c\x61\x74\x65\x5f\x78\x73\x6c\x74\x2e\x69\x6e\x63"))
(assert (str.in.re literal_0 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_0  "\x61" 0)) (str.len literal_0 )))
(check-sat)
(get-model)
