(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_2 () String)
(declare-fun x_3 () String)
(declare-fun literal_4 () String)
(declare-fun x_5 () String)
(declare-fun literal_6 () String)
(declare-fun x_7 () String)
(assert (= literal_2 "\x3c\x70\x3e"))
(assert (= x_3 (str.++ literal_2 sigmaStar_0)))
(assert (= literal_4 "\x20\x2d\x20\x3c\x61\x20\x68\x72\x65\x66\x3d\x5c\x22\x6d\x6f\x64\x5f\x70\x6c\x75\x67\x69\x6e\x73\x2e\x70\x68\x70\x3f\x61\x63\x74\x69\x6f\x6e\x3d\x69\x6e\x73\x74\x61\x6c\x6c\x26\x70\x6c\x75\x67\x69\x6e\x5f\x66\x69\x6c\x65\x3d"))
(assert (= x_5 (str.++ x_3 literal_4)))
(assert (= literal_6 "\x22\x3e\x49\x6e\x73\x74\x61\x6c\x6c\x3c\x2f\x61\x3e\x20\x5c\x6e"))
(assert (= x_7 (str.++ x_5 literal_6)))
(assert (str.in.re x_7 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_7 "\x61" 0)) (str.len x_7)))
(check-sat)
(get-model)
