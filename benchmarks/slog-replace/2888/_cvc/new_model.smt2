(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun literal_7 () String)
(declare-fun x_6 () String)
(declare-fun x_4 () String)
(declare-fun literal_5 () String)
(declare-fun x_8 () String)
(declare-fun literal_9 () String)
(declare-fun x_11 () String)
(declare-fun literal_10 () String)
(declare-fun x_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_14 () String)
(declare-fun sigmaStar_15 () String)
(assert (= x_4 (str.replace sigmaStar_0 "\x2f\x5b\x5e\x30\x2d\x39\x61\x2d\x7a\x5c\x2d\x5f\x2c\x5d\x2b\x2f\x69" "")))
(assert (= literal_7 "\x74\x68\x65\x6d\x65\x73\x2f"))
(assert (= literal_5 ""))
(assert (or (= x_6 x_4) (= x_6 literal_5)))
(assert (= x_8 (str.++ literal_7 x_6)))
(assert (= literal_9 "\x2f\x65\x64\x69\x74\x6f\x72\x5f\x74\x65\x6d\x70\x6c\x61\x74\x65"))
(assert (= x_11 (str.++ x_8 literal_9)))
(assert (= literal_10 ""))
(assert (= x_12 (str.++ x_11 literal_10)))
(assert (= literal_13 "\x2e\x6a\x73"))
(assert (= x_14 (str.++ x_12 literal_13)))
(assert (str.in.re sigmaStar_15 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_14 "\x61" 0)) (str.len x_14)))
(check-sat)
(get-model)
