(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_6 () String)
(declare-fun sigmaStar_7 () String)
(declare-fun literal_8 () String)
(declare-fun x_10 () String)
(declare-fun x_9 () String)
(declare-fun x_11 () String)
(declare-fun literal_12 () String)
(declare-fun x_13 () String)
(assert (= literal_8 "\x3c\x2f\x73\x70\x61\x6e\x3e\x5c\x6e\x3c\x73\x70\x61\x6e\x20\x63\x6c\x61\x73\x73\x3d\x27\x73\x6d\x61\x6c\x6c\x27\x3e"))
(assert (= x_10 (str.++ literal_8 sigmaStar_0)))
(assert (or (= x_9 sigmaStar_7) (= x_9 sigmaStar_6)))
(assert (= x_11 (str.++ x_10 x_9)))
(assert (= literal_12 "\x3c\x2f\x73\x70\x61\x6e\x3e"))
(assert (= x_13 (str.++ x_11 literal_12)))
(assert (str.in.re x_13 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_13 "\x61" 0)) (str.len x_13)))
(check-sat)
(get-model)
