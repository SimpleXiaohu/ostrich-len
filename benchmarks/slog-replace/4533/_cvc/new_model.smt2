(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_1 () String)
(declare-fun x_2 () String)
(declare-fun literal_3 () String)
(declare-fun x_4 () String)
(assert (= literal_1 "\x3c\x74\x64\x20\x77\x69\x64\x74\x68\x3d\x27"))
(assert (= x_2 (str.++ literal_1 sigmaStar_0)))
(assert (= literal_3 "\x20\x76\x61\x6c\x69\x67\x6e\x3d\x74\x6f\x70\x27\x20\x63\x6c\x61\x73\x73\x3d\x27\x73\x69\x64\x65\x2d\x62\x6f\x72\x64\x65\x72\x2d\x72\x69\x67\x68\x74\x27\x3e\x5c\x6e"))
(assert (= x_4 (str.++ x_2 literal_3)))
(assert (str.in.re x_4 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_4 "\x61" 0)) (str.len x_4)))
(check-sat)
(get-model)
