(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun literal_2 () String)
(assert (= literal_2 "\x2f\x6d\x6f\x64\x2f\x2f\x6d\x6f\x64\x65\x72\x72\x2e\x68\x74\x6d\x6c"))
(assert (str.in.re literal_2 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_2  "\x61" 0)) (str.len literal_2 )))
(check-sat)
(get-model)
