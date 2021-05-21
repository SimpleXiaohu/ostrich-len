(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_6 () String)
(assert (= literal_6 "\x3c\x69\x6e\x70\x75\x74\x20\x6e\x61\x6d\x65\x3d\x22\x74\x69\x6d\x65\x74\x6f\x72\x65\x73\x74\x72\x69\x63\x74\x22\x20\x74\x79\x70\x65\x3d\x22\x63\x68\x65\x63\x6b\x62\x6f\x78\x22\x20\x76\x61\x6c\x75\x65\x3d\x22\x31\x22\x20\x61\x6c\x74\x3d\x22\x73\x65\x61\x72\x63\x68\x64\x61\x74\x65\x74\x6f\x22\x20\x6f\x6e\x63\x6c\x69\x63\x6b\x3d\x22\x72\x65\x74\x75\x72\x6e\x20\x6c\x6f\x63\x6b\x6f\x70\x74\x69\x6f\x6e\x73\x28\x27\x73\x65\x61\x72\x63\x68\x27\x2c\x20\x27\x74\x69\x6d\x65\x74\x6f\x72\x65\x73\x74\x72\x69\x63\x74\x27\x2c\x20\x74\x69\x6d\x65\x74\x6f\x69\x74\x65\x6d\x73\x29\x22\x20\x2f\x3e\x20"))
(assert (str.in.re literal_6 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_6  "\x61" 0)) (str.len literal_6 )))
(check-sat)
(get-model)
