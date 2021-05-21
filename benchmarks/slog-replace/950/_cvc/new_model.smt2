(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun literal_2 () String)
(declare-fun x_6 () String)
(declare-fun epsilon () String)
(declare-fun literal_4 () String)
(declare-fun x_7 () String)
(declare-fun literal_8 () String)
(declare-fun x_9 () String)
(declare-fun x_10 () String)
(declare-fun x_11 () String)
(declare-fun literal_12 () String)
(declare-fun x_13 () String)
(assert (= literal_2 "\x2f\x6d\x6f\x64\x2f\x67\x6c\x6f\x73\x73\x61\x72\x79\x2f\x66\x6f\x72\x6d\x61\x74\x73\x2f"))
(assert (= epsilon ""))
(assert (= literal_4 "\x64\x69\x73\x70\x6c\x61\x79\x66\x6f\x72\x6d\x61\x74"))
(assert (or (= x_6 epsilon) (= x_6 literal_4)))
(assert (= x_7 (str.++ literal_2 x_6)))
(assert (= literal_8 "\x2f"))
(assert (= x_9 (str.++ x_7 literal_8)))
(assert (or (= x_10 epsilon) (= x_10 literal_4)))
(assert (= x_11 (str.++ x_9 x_10)))
(assert (= literal_12 "\x5f\x66\x6f\x72\x6d\x61\x74\x2e\x70\x68\x70"))
(assert (= x_13 (str.++ x_11 literal_12)))
(assert (str.in.re x_13 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_13 "\x61" 0)) (str.len x_13)))
(check-sat)
(get-model)
