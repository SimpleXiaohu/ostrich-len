(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun literal_0 () String)
(assert (= literal_0 "\x2f\x6c\x69\x62\x2f\x6c\x61\x6e\x67\x75\x61\x67\x65\x73\x2e\x6d\x64\x35"))
(assert (str.in.re literal_0 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_0  "\x61" 0)) (str.len literal_0 )))
(check-sat)
(get-model)