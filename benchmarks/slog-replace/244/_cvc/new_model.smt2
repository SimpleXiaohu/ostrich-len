(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_1 () String)
(declare-fun x_2 () String)
(declare-fun epsilon () String)
(declare-fun x_3 () String)
(declare-fun literal_4 () String)
(declare-fun x_5 () String)
(assert (= literal_1 "\x2f\x74\x68\x65\x6d\x65\x2f"))
(assert (= epsilon ""))
(assert (or (= x_2 epsilon) (= x_2 sigmaStar_0)))
(assert (= x_3 (str.++ literal_1 x_2)))
(assert (= literal_4 "\x2f\x75\x69\x2f\x73\x61\x72\x69\x73\x73\x61\x2e\x6a\x73"))
(assert (= x_5 (str.++ x_3 literal_4)))
(assert (str.in.re x_5 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_5 "\x61" 0)) (str.len x_5)))
(check-sat)
(get-model)