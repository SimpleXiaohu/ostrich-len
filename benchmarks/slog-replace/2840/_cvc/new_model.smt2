(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_5 () String)
(declare-fun sigmaStar_6 () String)
(declare-fun literal_8 () String)
(declare-fun x_7 () String)
(declare-fun epsilon () String)
(declare-fun x_9 () String)
(declare-fun literal_10 () String)
(declare-fun x_11 () String)
(assert (= literal_8 "\x2f\x62\x6c\x6f\x63\x6b\x73\x2f"))
(assert (= epsilon ""))
(assert (or (= x_7 epsilon) (= x_7 sigmaStar_6)))
(assert (= x_9 (str.++ literal_8 x_7)))
(assert (= literal_10 "\x2f\x63\x6f\x6e\x66\x69\x67\x5f\x69\x6e\x73\x74\x61\x6e\x63\x65\x2e\x68\x74\x6d\x6c"))
(assert (= x_11 (str.++ x_9 literal_10)))
(assert (str.in.re x_11 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_11 "\x61" 0)) (str.len x_11)))
(check-sat)
(get-model)
