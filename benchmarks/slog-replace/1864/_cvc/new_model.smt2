(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_7 () String)
(assert (= literal_7 "\x3c\x68\x33\x3e\x20\x2d\x3e\x20\x3c\x61\x20\x68\x72\x65\x66\x3d\x5c\x22\x2f\x63\x6f\x75\x72\x73\x65\x2f\x5c\x22\x3e\x63\x6f\x75\x72\x73\x65\x73\x3c\x2f\x61\x3e\x3c\x2f\x68\x33\x3e\x3c\x2f\x63\x65\x6e\x74\x65\x72\x3e\x5c\x6e"))
(assert (str.in.re literal_7 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_7  "\x61" 0)) (str.len literal_7 )))
(check-sat)
(get-model)
