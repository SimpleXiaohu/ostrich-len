(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_10 () String)
(declare-fun literal_17 () String)
(assert (= literal_17 "\x3c\x69\x6d\x67\x20\x73\x72\x63\x3d\x22\x2f\x75\x73\x65\x72\x2f\x70\x69\x78\x2e\x70\x68\x70\x2f\x2f\x66\x31\x2e\x6a\x70\x67\x22\x20\x62\x6f\x72\x64\x65\x72\x3d\x22\x30\x22\x20\x77\x69\x64\x74\x68\x3d\x22\x31\x30\x30\x22\x20\x68\x65\x69\x67\x68\x74\x3d\x22\x31\x30\x30\x22\x20\x61\x6c\x74\x3d\x22\x75\x73\x65\x72\x22\x20\x2f\x3e"))
(assert (str.in.re literal_17 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_17  "\x61" 0)) (str.len literal_17 )))
(check-sat)
(get-model)
