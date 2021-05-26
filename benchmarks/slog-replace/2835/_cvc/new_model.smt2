(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun literal_4 () String)
(declare-fun x_5 () String)
(declare-fun literal_6 () String)
(declare-fun x_7 () String)
(declare-fun x_8 () String)
(declare-fun literal_9 () String)
(declare-fun x_10 () String)
(assert (= literal_4 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x68\x69\x64\x64\x65\x6e\x22\x20\x6e\x61\x6d\x65\x3d\x22"))
(assert (= x_5 (str.++ literal_4 sigmaStar_2)))
(assert (= literal_6 "\x22\x20\x76\x61\x6c\x75\x65\x3d\x22"))
(assert (= x_7 (str.++ x_5 literal_6)))
(assert (= x_8 (str.++ x_7 sigmaStar_2)))
(assert (= literal_9 "\x22\x20\x2f\x3e"))
(assert (= x_10 (str.++ x_8 literal_9)))
(assert (str.in.re x_10 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_10 "\x61" 0)) (str.len x_10)))
(check-sat)
(get-model)