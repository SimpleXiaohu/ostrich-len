(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_8 () String)
(assert (= literal_8 "\x26\x6e\x62\x73\x70\x3b\x3c\x69\x6e\x70\x75\x74\x20\x74\x69\x74\x6c\x65\x3d\x5c\x22\x73\x65\x6c\x65\x63\x74\x5c\x22\x20\x74\x79\x70\x65\x3d\x5c\x22\x63\x68\x65\x63\x6b\x62\x6f\x78\x5c\x22\x20\x6e\x61\x6d\x65\x3d\x5c\x22\x71\x5c\x22\x20\x76\x61\x6c\x75\x65\x3d\x5c\x22\x31\x5c\x22\x20\x2f\x3e"))
(assert (str.in.re literal_8 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_8  "\x61" 0)) (str.len literal_8 )))
(check-sat)
(get-model)