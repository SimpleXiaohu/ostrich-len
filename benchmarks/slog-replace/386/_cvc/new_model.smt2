(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_11 () String)
(assert (= literal_11 "\x3c\x70\x3e\x3c\x64\x69\x76\x20\x61\x6c\x69\x67\x6e\x3d\x5c\x22\x63\x65\x6e\x74\x65\x72\x5c\x22\x20\x63\x6c\x61\x73\x73\x3d\x5c\x22\x6c\x65\x73\x73\x6f\x6e\x62\x75\x74\x74\x6f\x6e\x20\x73\x74\x61\x6e\x64\x61\x72\x64\x62\x75\x74\x74\x6f\x6e\x5c\x22\x3e\x3c\x61\x20\x68\x72\x65\x66\x3d\x5c\x22\x6a\x61\x76\x61\x73\x63\x72\x69\x70\x74\x3a\x64\x6f\x63\x75\x6d\x65\x6e\x74\x2e\x70\x61\x67\x65\x66\x6f\x72\x6d\x2e\x70\x61\x67\x65\x69\x64\x2e\x76\x61\x6c\x75\x65\x3d\x70\x61\x67\x65\x69\x64\x3b\x64\x6f\x63\x75\x6d\x65\x6e\x74\x2e\x70\x61\x67\x65\x66\x6f\x72\x6d\x2e\x73\x75\x62\x6d\x69\x74\x28\x29\x3b\x5c\x22\x3e\x72\x65\x76\x69\x65\x77\x71\x75\x65\x73\x74\x69\x6f\x6e\x62\x61\x63\x6b\x3c\x2f\x61\x3e\x3c\x2f\x64\x69\x76\x3e\x3c\x2f\x70\x3e\x5c\x6e"))
(assert (str.in.re literal_11 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_11  "\x61" 0)) (str.len literal_11 )))
(check-sat)
(get-model)