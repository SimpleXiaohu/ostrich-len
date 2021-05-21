(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_1 () String)
(declare-fun x_2 () String)
(declare-fun literal_3 () String)
(declare-fun x_4 () String)
(assert (= literal_1 "\x3c\x74\x61\x62\x6c\x65\x20\x63\x65\x6c\x6c\x70\x61\x64\x64\x69\x6e\x67\x3d\x30\x20\x63\x65\x6c\x6c\x73\x70\x61\x63\x69\x6e\x67\x3d\x30\x20\x62\x6f\x72\x64\x65\x72\x3d\x30\x20\x77\x69\x64\x74\x68\x3d\x27\x31\x30\x30\x25\x27\x20\x68\x65\x69\x67\x68\x74\x3d\x27\x38\x30\x27\x3e\x0d\x0a\x20\x3c\x74\x72\x3e\x0d\x0a\x20\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27\x62\x27\x20\x77\x69\x64\x74\x68\x3d\x27\x33\x30\x30\x27\x3e\x0d\x0a\x20\x20\x3c\x69\x6d\x67\x20\x73\x72\x63\x3d\x27\x2e\x2f\x69\x6d\x61\x67\x65\x73\x2f\x74\x69\x74\x6c\x65\x2e\x67\x69\x66\x27\x20\x68\x65\x69\x67\x68\x74\x3d\x27\x37\x35\x27\x20\x77\x69\x64\x74\x68\x3d\x27\x33\x30\x30\x27\x20\x2f\x3e\x0d\x0a\x20\x3c\x2f\x74\x64\x3e\x0d\x0a\x20\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27\x62\x27\x3e\x0d\x0a\x20\x20\x3c\x74\x61\x62\x6c\x65\x20\x63\x65\x6c\x6c\x70\x61\x64\x64\x69\x6e\x67\x3d\x30\x20\x63\x65\x6c\x6c\x73\x70\x61\x63\x69\x6e\x67\x3d\x30\x20\x62\x6f\x72\x64\x65\x72\x3d\x30\x20\x77\x69\x64\x74\x68\x3d\x27\x38\x30\x25\x27\x3e\x0d\x0a\x20\x20\x3c\x74\x72\x3e\x0d\x0a\x20\x20\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27\x62\x27\x3e\x0d\x0a\x20\x20\x20\x3c\x64\x69\x76\x20\x63\x6c\x61\x73\x73\x3d\x27\x79\x65\x6c\x6c\x6f\x77\x74\x65\x78\x74\x27\x20\x61\x6c\x69\x67\x6e\x3d\x27\x63\x65\x6e\x74\x65\x72\x27\x3e"))
(assert (= x_2 (str.++ literal_1 sigmaStar_0)))
(assert (= literal_3 "\x3c\x2f\x64\x69\x76\x3e\x0d\x0a\x20\x20\x3c\x2f\x74\x64\x3e\x0d\x0a\x20\x20\x3c\x2f\x74\x72\x3e\x0d\x0a\x20\x20\x3c\x2f\x74\x61\x62\x6c\x65\x3e\x0d\x0a\x20\x3c\x2f\x74\x64\x3e\x0d\x0a\x3c\x2f\x74\x72\x3e\x0d\x0a\x3c\x2f\x74\x61\x62\x6c\x65\x3e\x0d\x0a\x0d\x0a\x20\x3c\x74\x61\x62\x6c\x65\x20\x77\x69\x64\x74\x68\x3d\x27\x31\x30\x30\x25\x27\x20\x68\x65\x69\x67\x68\x74\x3d\x27\x38\x38\x25\x27\x20\x62\x6f\x72\x64\x65\x72\x3d\x30\x20\x63\x65\x6c\x6c\x73\x70\x61\x63\x69\x6e\x67\x3d\x30\x20\x63\x65\x6c\x6c\x70\x61\x64\x64\x69\x6e\x67\x3d\x30\x20\x61\x6c\x69\x67\x6e\x3d\x27\x63\x65\x6e\x74\x65\x72\x27\x3e\x0d\x0a\x20\x3c\x74\x72\x3e\x0d\x0a\x20\x20\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27\x62\x27\x20\x77\x69\x64\x74\x68\x3d\x27\x31\x33\x30\x27\x20\x68\x65\x69\x67\x68\x74\x3d\x31\x30\x3e\x3c\x65\x6d\x70\x74\x79\x3e\x3c\x2f\x74\x64\x3e\x0d\x0a\x20\x20\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27\x62\x27\x20\x77\x69\x64\x74\x68\x3d\x31\x30\x20\x62\x61\x63\x6b\x67\x72\x6f\x75\x6e\x64\x3d\x27\x2e\x2f\x69\x6d\x61\x67\x65\x73\x2f\x74\x6f\x70\x6c\x65\x66\x74\x2e\x67\x69\x66\x27\x3e\x3c\x65\x6d\x70\x74\x79\x3e\x3c\x2f\x74\x64\x3e\x0d\x0a\x20\x20\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27\x62\x27\x20\x68\x65\x69\x67\x68\x74\x3d\x31\x30\x20\x62\x61\x63\x6b\x67\x72\x6f\x75\x6e\x64\x3d\x27\x2e\x2f\x69\x6d\x61\x67\x65\x73\x2f\x74\x6f\x70\x2e\x67\x69\x66\x27\x3e\x3c\x65\x6d\x70\x74\x79\x3e\x3c\x2f\x74\x64\x3e\x0d\x0a\x20\x20\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27\x62\x27\x20\x77\x69\x64\x74\x68\x3d\x31\x30\x20\x62\x61\x63\x6b\x67\x72\x6f\x75\x6e\x64\x3d\x27\x2e\x2f\x69\x6d\x61\x67\x65\x73\x2f\x74\x6f\x70\x72\x69\x67\x68\x74\x2e\x67\x69\x66\x27\x3e\x3c\x65\x6d\x70\x74\x79\x3e\x3c\x2f\x74\x64\x3e\x0d\x0a\x20\x3c\x2f\x74\x72\x3e\x0d\x0a\x0d\x0a"))
(assert (= x_4 (str.++ x_2 literal_3)))
(assert (str.in.re x_4 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_4 "\x61" 0)) (str.len x_4)))
(check-sat)
(get-model)
