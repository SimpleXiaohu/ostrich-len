(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_5 () String)
(declare-fun x_4 () String)
(declare-fun literal_2 () String)
(declare-fun literal_3 () String)
(declare-fun x_6 () String)
(declare-fun literal_7 () String)
(declare-fun x_8 () String)
(declare-fun x_9 () String)
(declare-fun literal_10 () String)
(declare-fun x_11 () String)
(declare-fun x_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_14 () String)
(assert (= literal_5 "\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27"))
(assert (= literal_2 "\x74\x62\x6c\x31"))
(assert (= literal_3 "\x74\x62\x6c\x32"))
(assert (or (= x_4 literal_2) (= x_4 literal_3)))
(assert (= x_6 (str.++ literal_5 x_4)))
(assert (= literal_7 "\x27\x20\x73\x74\x79\x6c\x65\x3d\x27\x70\x61\x64\x64\x69\x6e\x67\x2d\x6c\x65\x66\x74\x3a\x31\x30\x70\x78\x3b\x70\x61\x64\x64\x69\x6e\x67\x2d\x72\x69\x67\x68\x74\x3a\x31\x30\x70\x78\x3b\x27\x3e\x3c\x73\x70\x61\x6e\x20\x63\x6c\x61\x73\x73\x3d\x27\x73\x6d\x61\x6c\x6c\x27\x3e\x3c\x61\x20\x68\x72\x65\x66\x3d\x27\x73\x65\x74\x74\x69\x6e\x67\x73\x5f\x6d\x69\x73\x63\x2e\x70\x68\x70"))
(assert (= x_8 (str.++ x_6 literal_7)))
(assert (= x_9 (str.++ x_8 sigmaStar_0)))
(assert (= literal_10 "\x27\x3e"))
(assert (= x_11 (str.++ x_9 literal_10)))
(assert (= x_12 (str.++ x_11 sigmaStar_1)))
(assert (= literal_13 "\x3c\x2f\x61\x3e\x3c\x2f\x73\x70\x61\x6e\x3e\x3c\x2f\x74\x64\x3e\x5c\x6e"))
(assert (= x_14 (str.++ x_12 literal_13)))
(assert (str.in.re x_14 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_14 "\x61" 0)) (str.len x_14)))
(check-sat)
(get-model)
