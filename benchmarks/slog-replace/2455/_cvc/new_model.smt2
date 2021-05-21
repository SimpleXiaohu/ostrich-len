(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_5 () String)
(assert (= literal_5 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x5c\x22\x62\x75\x74\x74\x6f\x6e\x5c\x22\x20\x6f\x6e\x63\x6c\x69\x63\x6b\x3d\x5c\x22\x63\x68\x65\x63\x6b\x61\x6c\x6c\x28\x29\x5c\x22\x20\x76\x61\x6c\x75\x65\x3d\x5c\x22\x73\x65\x6c\x65\x63\x74\x61\x6c\x6c\x5c\x22\x20\x2f\x3e\x5c\x6e"))
(assert (str.in.re literal_5 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_5  "\x61" 0)) (str.len literal_5 )))
(check-sat)
(get-model)
