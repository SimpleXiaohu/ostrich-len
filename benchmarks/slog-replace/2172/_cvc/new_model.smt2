(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun literal_6 () String)
(assert (= literal_6 "\x2f\x66\x69\x6c\x74\x65\x72\x2f\x66\x69\x6c\x74\x65\x72\x2f\x6a\x61\x76\x61\x73\x63\x72\x69\x70\x74\x2e\x70\x68\x70"))
(assert (str.in.re literal_6 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_6  "\x61" 0)) (str.len literal_6 )))
(check-sat)
(get-model)
