(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_5 () String)
(assert (= literal_5 "\x3c\x74\x68\x20\x63\x6c\x61\x73\x73\x3d\x22\x63\x35\x20\x68\x65\x61\x64\x65\x72\x22\x3e\x69\x6e\x66\x6f\x3c\x2f\x74\x68\x3e\x5c\x6e"))
(assert (str.in.re literal_5 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_5  "\x61" 0)) (str.len literal_5 )))
(check-sat)
(get-model)
