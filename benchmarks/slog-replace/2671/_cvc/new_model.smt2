(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun literal_0 () String)
(assert (= literal_0 "\x2f\x62\x61\x63\x6b\x75\x70\x2f\x64\x62\x2f\x6d\x69\x67\x72\x61\x74\x65\x32\x75\x74\x66\x38\x2e\x78\x6d\x6c"))
(assert (str.in.re literal_0 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_0  "\x61" 0)) (str.len literal_0 )))
(check-sat)
(get-model)
