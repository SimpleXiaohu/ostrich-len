(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_2 () String)
(declare-fun x_3 () String)
(declare-fun literal_4 () String)
(declare-fun x_5 () String)
(declare-fun x_6 () String)
(declare-fun literal_7 () String)
(declare-fun x_8 () String)
(assert (= literal_2 "\x3c\x73\x70\x61\x6e\x20\x63\x6c\x61\x73\x73\x3d\x27\x61\x6c\x74\x27\x3e"))
(assert (= x_3 (str.++ literal_2 sigmaStar_1)))
(assert (= literal_4 "\x3c\x2f\x73\x70\x61\x6e\x3e\x20"))
(assert (= x_5 (str.++ x_3 literal_4)))
(assert (= x_6 (str.++ x_5 sigmaStar_0)))
(assert (= literal_7 "\x3c\x62\x72\x3e\x5c\x6e"))
(assert (= x_8 (str.++ x_6 literal_7)))
(assert (str.in.re x_8 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_8 "\x61" 0)) (str.len x_8)))
(check-sat)
(get-model)
