(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun literal_0 () String)
(assert (= literal_0 "\x63\x6c\x61\x73\x73\x2e\x68\x74\x74\x70\x64\x6f\x77\x6e\x6c\x6f\x61\x64\x2e\x70\x68\x70"))
(assert (str.in.re literal_0 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_0  "\x61" 0)) (str.len literal_0 )))
(check-sat)
(get-model)
