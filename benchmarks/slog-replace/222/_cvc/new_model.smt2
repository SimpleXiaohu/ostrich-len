(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun literal_0 () String)
(assert (= literal_0 "\x2f\x71\x75\x65\x73\x74\x69\x6f\x6e\x2f\x74\x79\x70\x65\x2f\x64\x65\x73\x63\x72\x69\x70\x74\x69\x6f\x6e\x2f\x65\x64\x69\x74\x71\x75\x65\x73\x74\x69\x6f\x6e\x2e\x68\x74\x6d\x6c"))
(assert (str.in.re literal_0 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_0  "\x61" 0)) (str.len literal_0 )))
(check-sat)
(get-model)
