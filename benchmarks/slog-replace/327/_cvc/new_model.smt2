(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_4 () String)
(declare-fun sigmaStar_9 () String)
(declare-fun x_8 () String)
(declare-fun literal_3 () String)
(declare-fun literal_6 () String)
(declare-fun literal_5 () String)
(declare-fun literal_10 () String)
(declare-fun x_11 () String)
(declare-fun x_12 () String)
(assert (= literal_3 "\x77\x64\x69\x72"))
(assert (= literal_6 "\x2f"))
(assert (= literal_5 "\x2f"))
(assert (or (= x_8 literal_3) (= x_8 literal_6) (= x_8 literal_5)))
(assert (= literal_10 "\x2f"))
(assert (= x_11 (str.++ x_8 literal_10)))
(assert (= x_12 (str.++ x_11 sigmaStar_9)))
(assert (str.in.re x_12 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_12 "\x61" 0)) (str.len x_12)))
(check-sat)
(get-model)
