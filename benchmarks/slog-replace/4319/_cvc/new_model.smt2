(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun literal_10 () String)
(declare-fun x_13 () String)
(declare-fun x_18 () String)
(declare-fun sigmaStar_17 () String)
(declare-fun sigmaStar_24 () String)
(declare-fun literal_25 () String)
(declare-fun x_26 () String)
(declare-fun literal_27 () String)
(declare-fun x_28 () String)
(assert (= literal_10 "\x42\x79\x74\x65\x73"))
(assert (= x_13 (str.++ sigmaStar_1 literal_10)))
(assert (= x_18 (str.replace sigmaStar_2 "\x2c" "\x20")))
(assert (= literal_25 "\x3c\x73\x70\x61\x6e\x20\x63\x6c\x61\x73\x73\x3d\x27\x73\x6d\x61\x6c\x6c\x32\x27\x3e"))
(assert (= x_26 (str.++ literal_25 sigmaStar_24)))
(assert (= literal_27 "\x3c\x2f\x73\x70\x61\x6e\x3e"))
(assert (= x_28 (str.++ x_26 literal_27)))
(assert (str.in.re x_28 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_28 "\x61" 0)) (str.len x_28)))
(check-sat)
(get-model)
