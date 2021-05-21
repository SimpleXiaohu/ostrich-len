(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun x_2 () String)
(declare-fun epsilon () String)
(declare-fun x_3 () String)
(declare-fun literal_4 () String)
(declare-fun x_5 () String)
(assert (= epsilon ""))
(assert (or (= x_2 epsilon)))
(assert (= x_3 (str.++ sigmaStar_0 x_2)))
(assert (= literal_4 "\x3c\x2f\x73\x70\x61\x6e\x3e\x3c\x62\x72\x3e\x5c\x6e"))
(assert (= x_5 (str.++ x_3 literal_4)))
(assert (str.in.re x_5 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_5 "\x61" 0)) (str.len x_5)))
(check-sat)
(get-model)
