(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_7 () String)
(assert (= literal_7 "\x3c\x70\x3e\x3c\x66\x6f\x6e\x74\x20\x73\x69\x7a\x65\x3d\x5c\x22\x31\x5c\x22\x3e\x3c\x69\x6d\x67\x20\x61\x6c\x69\x67\x6e\x3d\x5c\x22\x6d\x69\x64\x64\x6c\x65\x5c\x22\x20\x61\x6c\x74\x3d\x5c\x22\x5c\x22\x20\x68\x65\x69\x67\x68\x74\x3d\x5c\x22\x31\x36\x5c\x22\x20\x77\x69\x64\x74\x68\x3d\x5c\x22\x31\x36\x5c\x22\x20\x62\x6f\x72\x64\x65\x72\x3d\x5c\x22\x30\x5c\x22\x20\x73\x72\x63\x3d\x5c\x22\x2f\x69\x2f\x6b\x65\x79\x2e\x67\x69\x66\x5c\x22\x20\x2f\x3e\x3c\x2f\x61\x3e\x26\x6e\x62\x73\x70\x3b\x72\x65\x71\x75\x69\x72\x65\x73\x6b\x65\x79\x3c\x2f\x66\x6f\x6e\x74\x3e\x3c\x2f\x70\x3e"))
(assert (str.in.re literal_7 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_7  "\x61" 0)) (str.len literal_7 )))
(check-sat)
(get-model)
