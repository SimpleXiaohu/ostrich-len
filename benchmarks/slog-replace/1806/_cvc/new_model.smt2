(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun literal_0 () String)
(assert (= literal_0 "\x2f\x61\x75\x74\x68\x2f\x63\x61\x73\x2f\x69\x6e\x64\x65\x78\x5f\x66\x6f\x72\x6d\x2e\x68\x74\x6d\x6c"))
(assert (str.in.re literal_0 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_0  "\x61" 0)) (str.len literal_0 )))
(check-sat)
(get-model)
