(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_6 () String)
(declare-fun x_8 () String)
(declare-fun literal_9 () String)
(declare-fun x_10 () String)
(declare-fun literal_7 () String)
(declare-fun x_11 () String)
(declare-fun literal_12 () String)
(declare-fun x_13 () String)
(assert (= literal_6 "\x7c\x20\x3c\x61\x20\x68\x72\x65\x66\x3d\x22\x2f\x62\x6c\x6f\x67\x2f\x65\x64\x69\x74\x2e\x70\x68\x70\x3f\x61\x63\x74\x3d\x64\x65\x6c\x26\x61\x6d\x70\x3b\x65\x64\x69\x74\x69\x64\x3d\x26\x61\x6d\x70\x3b\x73\x65\x73\x73\x6b\x65\x79\x3d"))
(assert (= x_8 (str.++ literal_6 sigmaStar_0)))
(assert (= literal_9 "\x22\x3e"))
(assert (= x_10 (str.++ x_8 literal_9)))
(assert (= literal_7 "\x64\x65\x6c\x65\x74\x65"))
(assert (= x_11 (str.++ x_10 literal_7)))
(assert (= literal_12 "\x3c\x2f\x61\x3e"))
(assert (= x_13 (str.++ x_11 literal_12)))
(assert (str.in.re x_13 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_13 "\x61" 0)) (str.len x_13)))
(check-sat)
(get-model)
