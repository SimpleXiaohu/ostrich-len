(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_4 () String)
(declare-fun x_10 () String)
(declare-fun literal_5 () String)
(declare-fun literal_11 () String)
(declare-fun x_12 () String)
(assert (= literal_5 ""))
(assert (or (= x_10 sigmaStar_3) (= x_10 sigmaStar_2) (= x_10 sigmaStar_1) (= x_10 sigmaStar_0) (= x_10 literal_5) (= x_10 sigmaStar_4)))
(assert (= literal_11 "\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x5c\x6e"))
(assert (= x_12 (str.++ x_10 literal_11)))
(assert (str.in.re x_12 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_12 "\x61" 0)) (str.len x_12)))
(check-sat)
(get-model)
