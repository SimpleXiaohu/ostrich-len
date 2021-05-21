(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_2 () String)
(declare-fun x_4 () String)
(declare-fun x_7 () String)
(declare-fun literal_3 () String)
(declare-fun literal_8 () String)
(declare-fun x_9 () String)
(declare-fun literal_6 () String)
(declare-fun x_10 () String)
(assert (= literal_2 "\x2f\x6c\x61\x6e\x67\x2f"))
(assert (= x_4 (str.++ literal_2 sigmaStar_1)))
(assert (= literal_3 "\x2f\x6c\x61\x6e\x67\x2f\x65\x6e\x5f\x75\x74\x66\x38"))
(assert (or (= x_7 literal_3) (= x_7 x_4)))
(assert (= literal_8 "\x2f"))
(assert (= x_9 (str.++ x_7 literal_8)))
(assert (= literal_6 "\x63\x75\x72\x72\x65\x6e\x74\x66\x69\x6c\x65"))
(assert (= x_10 (str.++ x_9 literal_6)))
(assert (str.in.re x_10 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_10 "\x61" 0)) (str.len x_10)))
(check-sat)
(get-model)
