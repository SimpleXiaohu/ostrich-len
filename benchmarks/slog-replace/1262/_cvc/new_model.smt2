(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_6 () String)
(assert (= literal_6 "\x3c\x74\x68\x20\x63\x6c\x61\x73\x73\x3d\x22\x68\x65\x61\x64\x65\x72\x20\x74\x6f\x70\x69\x63\x22\x3e\x64\x69\x73\x63\x75\x73\x73\x69\x6f\x6e\x3c\x2f\x74\x68\x3e"))
(assert (str.in.re literal_6 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_6  "\x61" 0)) (str.len literal_6 )))
(check-sat)
(get-model)
