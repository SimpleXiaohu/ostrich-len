(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun literal_9 () String)
(assert (= literal_9 "\x3c\x74\x64\x20\x73\x74\x79\x6c\x65\x3d\x22\x77\x69\x64\x74\x68\x3a\x20\x31\x38\x30\x70\x78\x3b\x22\x20\x69\x64\x3d\x22\x72\x69\x67\x68\x74\x2d\x63\x6f\x6c\x75\x6d\x6e\x22\x3e"))
(assert (str.in.re literal_9 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_9  "\x61" 0)) (str.len literal_9 )))
(check-sat)
(get-model)
