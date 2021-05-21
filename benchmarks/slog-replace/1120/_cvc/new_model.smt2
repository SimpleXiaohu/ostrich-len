(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_6 () String)
(assert (= literal_6 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x73\x75\x62\x6d\x69\x74\x22\x20\x76\x61\x6c\x75\x65\x3d\x22\x64\x69\x73\x61\x67\x72\x65\x65\x77\x69\x74\x68\x74\x68\x69\x73\x61\x73\x73\x65\x73\x73\x6d\x65\x6e\x74\x22\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x6f\x6e\x63\x6c\x69\x63\x6b\x3d\x22\x64\x6f\x63\x75\x6d\x65\x6e\x74\x2e\x61\x73\x73\x65\x73\x73\x6d\x65\x6e\x74\x66\x6f\x72\x6d\x2e\x61\x63\x74\x69\x6f\x6e\x2e\x76\x61\x6c\x75\x65\x3d\x27\x61\x64\x64\x63\x6f\x6d\x6d\x65\x6e\x74\x27\x3b\x64\x6f\x63\x75\x6d\x65\x6e\x74\x2e\x61\x73\x73\x65\x73\x73\x6d\x65\x6e\x74\x66\x6f\x72\x6d\x2e\x73\x75\x62\x6d\x69\x74\x28\x29\x3b\x22\x20\x2f\x3e\x5c\x6e"))
(assert (str.in.re literal_6 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_6  "\x61" 0)) (str.len literal_6 )))
(check-sat)
(get-model)
