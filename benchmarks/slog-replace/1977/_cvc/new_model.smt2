(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun literal_5 () String)
(declare-fun x_6 () String)
(declare-fun literal_7 () String)
(declare-fun x_8 () String)
(declare-fun literal_9 () String)
(declare-fun x_10 () String)
(assert (= literal_5 "\x3c\x61\x20\x68\x72\x65\x66\x3d\x5c\x22\x63\x61\x74\x65\x67\x6f\x72\x79\x2e\x70\x68\x70\x3f\x69\x64\x3d\x26\x61\x6d\x70\x3b\x70\x61\x67\x65\x3d"))
(assert (= x_6 (str.++ literal_5 sigmaStar_1)))
(assert (= literal_7 "\x26\x61\x6d\x70\x3b\x73\x65\x73\x73\x6b\x65\x79\x3d"))
(assert (= x_8 (str.++ x_6 literal_7)))
(assert (= literal_9 "\x5c\x22\x3e"))
(assert (= x_10 (str.++ x_8 literal_9)))
(assert (str.in.re x_10 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_10 "\x61" 0)) (str.len x_10)))
(check-sat)
(get-model)
