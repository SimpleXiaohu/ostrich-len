(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun x_2 () String)
(declare-fun literal_1 () String)
(assert (= literal_1 "\x36"))
(assert (or (= x_2 sigmaStar_0) (= x_2 literal_1)))
(assert (str.in.re x_2 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_2 "\x61" 0)) (str.len x_2)))
(check-sat)
(get-model)
