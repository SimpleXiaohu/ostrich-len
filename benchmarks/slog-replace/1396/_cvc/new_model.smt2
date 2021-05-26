(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_3 () String)
(declare-fun x_4 () String)
(declare-fun literal_2 () String)
(declare-fun x_5 () String)
(declare-fun literal_6 () String)
(declare-fun x_8 () String)
(declare-fun x_9 () String)
(declare-fun literal_7 () String)
(declare-fun x_10 () String)
(declare-fun literal_11 () String)
(declare-fun x_12 () String)
(assert (= literal_3 "\x3c\x66\x72\x61\x6d\x65\x73\x65\x74"))
(assert (= literal_2 ""))
(assert (or (= x_4 literal_2) (= x_4 sigmaStar_1)))
(assert (= x_5 (str.++ literal_3 x_4)))
(assert (= literal_6 "\x3e"))
(assert (= x_8 (str.++ x_5 literal_6)))
(assert (= literal_7 ""))
(assert (or (= x_9 sigmaStar_0) (= x_9 literal_7)))
(assert (= x_10 (str.++ x_8 x_9)))
(assert (= literal_11 "\x3c\x2f\x66\x72\x61\x6d\x65\x73\x65\x74\x3e\x5c\x6e"))
(assert (= x_12 (str.++ x_10 literal_11)))
(assert (str.in.re x_12 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_12 "\x61" 0)) (str.len x_12)))
(check-sat)
(get-model)