(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_6 () String)
(declare-fun x_8 () String)
(declare-fun literal_2 () String)
(declare-fun literal_3 () String)
(declare-fun literal_5 () String)
(declare-fun x_9 () String)
(declare-fun literal_10 () String)
(declare-fun x_11 () String)
(declare-fun x_12 () String)
(assert (= literal_6 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x27\x63\x68\x65\x63\x6b\x62\x6f\x78\x27\x20\x6e\x61\x6d\x65\x3d\x27\x64\x69\x73\x61\x62\x6c\x65\x5f\x73\x6d\x69\x6c\x65\x79\x73\x27\x20\x76\x61\x6c\x75\x65\x3d\x27\x31\x27"))
(assert (= literal_2 ""))
(assert (= literal_3 "\x20\x63\x68\x65\x63\x6b\x65\x64"))
(assert (= literal_5 ""))
(assert (or (= x_8 literal_2) (= x_8 literal_3) (= x_8 sigmaStar_1) (= x_8 literal_5)))
(assert (= x_9 (str.++ literal_6 x_8)))
(assert (= literal_10 "\x3e"))
(assert (= x_11 (str.++ x_9 literal_10)))
(assert (= x_12 (str.++ x_11 sigmaStar_0)))
(assert (str.in.re x_12 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_12 "\x61" 0)) (str.len x_12)))
(check-sat)
(get-model)
