(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_16 () String)
(assert (= literal_16 "\x3c\x61\x20\x74\x69\x74\x6c\x65\x3d\x22\x61\x73\x73\x69\x67\x6e\x74\x65\x61\x63\x68\x65\x72\x73\x22\x20\x68\x72\x65\x66\x3d\x22\x2f\x63\x6f\x75\x72\x73\x65\x2f\x74\x65\x61\x63\x68\x65\x72\x2e\x70\x68\x70\x3f\x69\x64\x3d\x22\x3e\x3c\x69\x6d\x67\x20\x73\x72\x63\x3d\x22\x2f\x74\x2f\x75\x73\x65\x72\x2e\x67\x69\x66\x22\x20\x68\x65\x69\x67\x68\x74\x3d\x22\x31\x31\x22\x20\x77\x69\x64\x74\x68\x3d\x22\x31\x31\x22\x20\x62\x6f\x72\x64\x65\x72\x3d\x22\x30\x22\x20\x61\x6c\x74\x3d\x22\x61\x73\x73\x69\x67\x6e\x74\x65\x61\x63\x68\x65\x72\x73\x22\x20\x2f\x3e\x3c\x2f\x61\x3e\x20"))
(assert (str.in.re literal_16 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_16  "\x61" 0)) (str.len literal_16 )))
(check-sat)
(get-model)
