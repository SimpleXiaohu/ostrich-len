(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun literal_11 () String)
(assert (= literal_11 "\x64\x69\x73\x63\x75\x73\x73\x69\x6f\x6e\x3a\x20\x75\x73\x65\x72"))
(assert (str.in.re literal_11 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_11  "\x61" 0)) (str.len literal_11 )))
(check-sat)
(get-model)
