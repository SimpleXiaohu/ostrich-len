(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_14 () String)
(assert (= literal_14 "\x3c\x61\x20\x74\x69\x74\x6c\x65\x3d\x22\x72\x65\x73\x74\x6f\x72\x65\x22\x20\x68\x72\x65\x66\x3d\x22\x2e\x2e\x2f\x66\x69\x6c\x65\x73\x2f\x69\x6e\x64\x65\x78\x2e\x70\x68\x70\x3f\x69\x64\x3d\x26\x61\x6d\x70\x3b\x77\x64\x69\x72\x3d\x2f\x62\x61\x63\x6b\x75\x70\x64\x61\x74\x61\x22\x3e\x3c\x69\x6d\x67\x20\x73\x72\x63\x3d\x22\x2f\x74\x2f\x72\x65\x73\x74\x6f\x72\x65\x2e\x67\x69\x66\x22\x20\x68\x65\x69\x67\x68\x74\x3d\x22\x31\x31\x22\x20\x77\x69\x64\x74\x68\x3d\x22\x31\x31\x22\x20\x62\x6f\x72\x64\x65\x72\x3d\x22\x30\x22\x20\x61\x6c\x74\x3d\x22\x72\x65\x73\x74\x6f\x72\x65\x22\x20\x2f\x3e\x3c\x2f\x61\x3e\x20"))
(assert (str.in.re literal_14 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_14  "\x61" 0)) (str.len literal_14 )))
(check-sat)
(get-model)
