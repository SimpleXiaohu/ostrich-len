(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun literal_13 () String)
(declare-fun x_12 () String)
(declare-fun literal_8 () String)
(declare-fun literal_9 () String)
(declare-fun literal_6 () String)
(declare-fun literal_4 () String)
(declare-fun x_14 () String)
(declare-fun literal_15 () String)
(declare-fun x_16 () String)
(assert (= literal_13 "\x3c\x64\x69\x76\x20\x63\x6c\x61\x73\x73\x3d\x22\x61\x75\x64\x69\x65\x6e\x63\x65\x22\x3e"))
(assert (= literal_8 "\x70\x75\x62\x6c\x69\x73\x68\x74\x6f\x6e\x6f\x6f\x6e\x65"))
(assert (= literal_9 ""))
(assert (= literal_6 "\x70\x75\x62\x6c\x69\x73\x68\x74\x6f\x77\x6f\x72\x6c\x64"))
(assert (= literal_4 "\x70\x75\x62\x6c\x69\x73\x68\x74\x6f\x73\x69\x74\x65"))
(assert (or (= x_12 literal_8) (= x_12 literal_9) (= x_12 literal_6) (= x_12 literal_4)))
(assert (= x_14 (str.++ literal_13 x_12)))
(assert (= literal_15 "\x3c\x2f\x64\x69\x76\x3e"))
(assert (= x_16 (str.++ x_14 literal_15)))
(assert (str.in.re x_16 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_16 "\x61" 0)) (str.len x_16)))
(check-sat)
(get-model)
