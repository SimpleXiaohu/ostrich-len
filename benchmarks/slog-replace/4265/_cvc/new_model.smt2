(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_5 () String)
(declare-fun x_6 () String)
(declare-fun literal_2 () String)
(declare-fun literal_3 () String)
(declare-fun x_7 () String)
(declare-fun literal_8 () String)
(declare-fun x_9 () String)
(declare-fun x_10 () String)
(declare-fun literal_11 () String)
(declare-fun x_12 () String)
(assert (= literal_5 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x27\x63\x68\x65\x63\x6b\x62\x6f\x78\x27\x20\x6e\x61\x6d\x65\x3d\x27\x64\x65\x6c\x65\x74\x65\x5f\x61\x74\x74\x61\x63\x68\x27\x20\x76\x61\x6c\x75\x65\x3d\x27\x31\x27"))
(assert (= literal_2 "\x20\x63\x68\x65\x63\x6b\x65\x64"))
(assert (= literal_3 ""))
(assert (or (= x_6 literal_2) (= x_6 literal_3) (= x_6 sigmaStar_1)))
(assert (= x_7 (str.++ literal_5 x_6)))
(assert (= literal_8 "\x3e"))
(assert (= x_9 (str.++ x_7 literal_8)))
(assert (= x_10 (str.++ x_9 sigmaStar_0)))
(assert (= literal_11 "\x5c\x6e"))
(assert (= x_12 (str.++ x_10 literal_11)))
(assert (str.in.re x_12 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_12 "\x61" 0)) (str.len x_12)))
(check-sat)
(get-model)
