(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun literal_3 () String)
(declare-fun x_4 () String)
(declare-fun literal_5 () String)
(declare-fun x_6 () String)
(declare-fun x_7 () String)
(declare-fun literal_8 () String)
(declare-fun x_9 () String)
(declare-fun x_10 () String)
(declare-fun literal_11 () String)
(declare-fun x_12 () String)
(assert (= literal_3 "\x3c\x61\x20\x68\x72\x65\x66\x3d\x27\x76\x69\x65\x77\x66\x6f\x72\x75\x6d\x2e\x70\x68\x70\x3f\x66\x6f\x72\x75\x6d\x5f\x69\x64\x3d"))
(assert (= x_4 (str.++ literal_3 sigmaStar_1)))
(assert (= literal_5 "\x27\x3e"))
(assert (= x_6 (str.++ x_4 literal_5)))
(assert (= x_7 (str.++ x_6 sigmaStar_0)))
(assert (= literal_8 "\x3c\x2f\x61\x3e\x20\x7c\x0d\x0a\x3c\x61\x20\x68\x72\x65\x66\x3d\x27\x69\x6e\x64\x65\x78\x2e\x70\x68\x70\x27\x3e"))
(assert (= x_9 (str.++ x_7 literal_8)))
(assert (= x_10 (str.++ x_9 sigmaStar_2)))
(assert (= literal_11 "\x3c\x2f\x61\x3e\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x5c\x6e\x3c\x2f\x63\x65\x6e\x74\x65\x72\x3e\x5c\x6e"))
(assert (= x_12 (str.++ x_10 literal_11)))
(assert (str.in.re x_12 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_12 "\x61" 0)) (str.len x_12)))
(check-sat)
(get-model)
