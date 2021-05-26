(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_7 () String)
(declare-fun literal_8 () String)
(declare-fun x_9 () String)
(assert (= literal_8 "\x3c\x62\x72\x3e\x5c\x6e"))
(assert (= x_9 (str.++ sigmaStar_7 literal_8)))
(assert (str.in.re x_9 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_9 "\x61" 0)) (str.len x_9)))
(check-sat)
(get-model)