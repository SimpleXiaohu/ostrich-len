(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_1 () String)
(assert (= literal_1 "\x2f\x6d\x61\x63\x72\x6f\x73\x2f"))
(assert (str.in.re literal_1 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_1  "\x61" 0)) (str.len literal_1 )))
(check-sat)
(get-model)