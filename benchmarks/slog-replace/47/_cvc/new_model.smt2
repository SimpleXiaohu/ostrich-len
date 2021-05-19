(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_7 () String)
(declare-fun x_4 () String)
(declare-fun sigmaStar_11 () String)
(declare-fun x_8 () String)
(declare-fun literal_13 () String)
(declare-fun x_12 () String)
(declare-fun x_14 () String)
(declare-fun literal_15 () String)
(declare-fun x_16 () String)
(assert (= x_4 (str.replace sigmaStar_0 "\x2f\x5b\x4a\x6a\x5d\x5b\x41\x61\x5d\x5b\x56\x76\x5d\x5b\x41\x61\x5d\x5b\x53\x73\x5d\x5b\x43\x63\x5d\x5b\x52\x72\x5d\x5b\x49\x69\x5d\x5b\x50\x70\x5d\x5b\x54\x74\x5d\x2f" "\x6a\x61\x76\x61\x20\x73\x63\x72\x69\x70\x74")))
(assert (= x_8 (str.replace x_4 "\x2f\x3c\x2f" "\x26\x6c\x74\x3b")))
(assert (= x_12 (str.replace x_8 "\x2f\x3e\x2f" "\x26\x67\x74\x3b")))
(assert (= literal_13 "\x3c\x70\x3e\x54\x69\x74\x6c\x65\x3a\x3c\x62\x72\x20\x2f\x3e"))
(assert (= x_14 (str.++ literal_13 x_12)))
(assert (= literal_15 "\x3c\x2f\x70\x3e"))
(assert (= x_16 (str.++ x_14 literal_15)))
(assert (str.in.re x_16 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_16 "\x61" 0)) (str.len x_16)))
(check-sat)
(get-model)
