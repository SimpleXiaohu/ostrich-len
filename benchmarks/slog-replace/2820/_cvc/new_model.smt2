(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_5 () String)
(declare-fun literal_3 () String)
(declare-fun x_6 () String)
(declare-fun sigmaStar_9 () String)
(declare-fun literal_7 () String)
(declare-fun x_10 () String)
(declare-fun sigmaStar_13 () String)
(declare-fun literal_11 () String)
(declare-fun x_14 () String)
(declare-fun sigmaStar_19 () String)
(declare-fun x_21 () String)
(declare-fun literal_2 () String)
(assert (= literal_3 "\x2f\x44\x65\x66\x61\x75\x6c\x74\x2e\x68\x74\x6d"))
(assert (= x_6 (str.++ sigmaStar_5 literal_3)))
(assert (= literal_7 "\x2f\x69\x6e\x64\x65\x78\x2e\x68\x74\x6d\x6c"))
(assert (= x_10 (str.++ sigmaStar_9 literal_7)))
(assert (= literal_11 "\x2f\x69\x6e\x64\x65\x78\x2e\x68\x74\x6d"))
(assert (= x_14 (str.++ sigmaStar_13 literal_11)))
(assert (= literal_2 "\x66\x69\x6c\x65\x2e\x70\x68\x70"))
(assert (or (= x_21 x_10) (= x_21 literal_2) (= x_21 x_14) (= x_21 x_6) (= x_21 sigmaStar_19)))
(assert (str.in.re x_21 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_21 "\x61" 0)) (str.len x_21)))
(check-sat)
(get-model)
