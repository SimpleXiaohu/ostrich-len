(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun literal_3 () String)
(declare-fun x_4 () String)
(declare-fun literal_5 () String)
(declare-fun x_6 () String)
(declare-fun x_7 () String)
(declare-fun literal_8 () String)
(declare-fun x_9 () String)
(declare-fun x_10 () String)
(declare-fun literal_11 () String)
(declare-fun x_12 () String)
(assert (= literal_3 "\x20\x20\x20\x20\x20\x3c\x2f\x73\x65\x6c\x65\x63\x74\x3e\x0d\x0a\x09\x3c\x2f\x74\x64\x3e\x0d\x0a\x20\x20\x20\x3c\x2f\x74\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x2f\x74\x61\x62\x6c\x65\x3e\x0d\x0a\x0d\x0a\x20\x20\x20\x3c\x62\x72\x3e\x0d\x0a\x0d\x0a\x20\x20\x20\x3c\x74\x61\x62\x6c\x65\x20\x63\x65\x6c\x6c\x70\x61\x64\x64\x69\x6e\x67\x3d\x27\x30\x27\x20\x62\x6f\x72\x64\x65\x72\x3d\x27\x30\x27\x20\x61\x6c\x69\x67\x6e\x3d\x27\x63\x65\x6e\x74\x65\x72\x27\x20\x77\x69\x64\x74\x68\x3d\x27\x35\x35\x30\x27\x3e\x0d\x0a\x20\x20\x20\x3c\x74\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x74\x64\x3e\x0d\x0a\x09\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x27\x62\x75\x74\x74\x6f\x6e\x27\x20\x76\x61\x6c\x75\x65\x3d\x27\x45\x64\x69\x74\x20\x70\x61\x72\x65\x6e\x74\x27\x20\x6f\x6e\x43\x6c\x69\x63\x6b\x3d\x27\x64\x6f\x63\x75\x6d\x65\x6e\x74\x2e\x65\x64\x69\x74\x70\x61\x72\x65\x6e\x74\x2e\x65\x64\x69\x74\x70\x61\x72\x65\x6e\x74\x2e\x76\x61\x6c\x75\x65\x3d\x31\x3b\x64\x6f\x63\x75\x6d\x65\x6e\x74\x2e\x65\x64\x69\x74\x70\x61\x72\x65\x6e\x74\x2e\x70\x61\x67\x65\x32\x2e\x76\x61\x6c\x75\x65\x3d\x32\x32\x3b\x64\x6f\x63\x75\x6d\x65\x6e\x74\x2e\x65\x64\x69\x74\x70\x61\x72\x65\x6e\x74\x2e\x73\x75\x62\x6d\x69\x74\x28\x29\x3b\x27\x3e\x0d\x0a\x09\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x27\x62\x75\x74\x74\x6f\x6e\x27\x20\x76\x61\x6c\x75\x65\x3d\x27\x43\x61\x6e\x63\x65\x6c\x27\x20\x6f\x6e\x43\x6c\x69\x63\x6b\x3d\x27\x64\x6f\x63\x75\x6d\x65\x6e\x74\x2e\x65\x64\x69\x74\x70\x61\x72\x65\x6e\x74\x2e\x70\x61\x67\x65\x32\x2e\x76\x61\x6c\x75\x65\x3d\x32\x32\x3b\x64\x6f\x63\x75\x6d\x65\x6e\x74\x2e\x65\x64\x69\x74\x70\x61\x72\x65\x6e\x74\x2e\x73\x75\x62\x6d\x69\x74\x28\x29\x3b\x27\x3e\x0d\x0a\x20\x20\x20\x3c\x2f\x74\x64\x3e\x0d\x0a\x20\x20\x20\x3c\x2f\x74\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x2f\x74\x61\x62\x6c\x65\x3e\x0d\x0a\x0d\x0a\x20\x20\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x27\x68\x69\x64\x64\x65\x6e\x27\x20\x6e\x61\x6d\x65\x3d\x27\x65\x64\x69\x74\x70\x61\x72\x65\x6e\x74\x27\x3e\x0d\x0a\x20\x20\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x27\x68\x69\x64\x64\x65\x6e\x27\x20\x6e\x61\x6d\x65\x3d\x27\x70\x61\x72\x65\x6e\x74\x69\x64\x27\x20\x76\x61\x6c\x75\x65\x3d\x27"))
(assert (= x_4 (str.++ literal_3 sigmaStar_1)))
(assert (= literal_5 "\x3e\x0d\x0a\x20\x20\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x68\x69\x64\x64\x65\x6e\x27\x20\x6e\x61\x6d\x65\x3d\x27\x70\x61\x67\x65\x32\x27\x20\x76\x61\x6c\x75\x65\x3d\x27"))
(assert (= x_6 (str.++ x_4 literal_5)))
(assert (= x_7 (str.++ x_6 sigmaStar_2)))
(assert (= literal_8 "\x3e\x0d\x0a\x20\x20\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x68\x69\x64\x64\x65\x6e\x27\x20\x6e\x61\x6d\x65\x3d\x27\x6c\x6f\x67\x6f\x75\x74\x27\x3e\x0d\x0a\x20\x20\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x27\x68\x69\x64\x64\x65\x6e\x27\x20\x6e\x61\x6d\x65\x3d\x27\x70\x61\x67\x65\x27\x20\x76\x61\x6c\x75\x65\x3d\x27"))
(assert (= x_9 (str.++ x_7 literal_8)))
(assert (= x_10 (str.++ x_9 sigmaStar_0)))
(assert (= literal_11 "\x3e\x0d\x0a\x0d\x0a\x20\x3c\x2f\x66\x6f\x72\x6d\x3e\x0d\x0a\x0d\x0a\x20\x3c\x74\x61\x62\x6c\x65\x20\x77\x69\x64\x74\x68\x3d\x35\x32\x30\x27\x20\x62\x6f\x72\x64\x65\x72\x3d\x30\x20\x63\x65\x6c\x6c\x73\x70\x61\x63\x69\x6e\x67\x3d\x30\x20\x63\x65\x6c\x6c\x70\x61\x64\x64\x69\x6e\x67\x3d\x30\x20\x68\x65\x69\x67\x68\x74\x3d\x31\x3e\x0d\x0a\x20\x20\x3c\x74\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x74\x64\x20\x76\x61\x6c\x69\x67\x6e\x3d\x27\x74\x6f\x70\x27\x3e\x0d\x0a\x09\x26\x6e\x62\x73\x70\x3b\x0d\x0a\x20\x20\x20\x3c\x2f\x74\x64\x3e\x0d\x0a\x20\x20\x3c\x2f\x74\x72\x3e\x0d\x0a\x20\x3c\x2f\x74\x61\x62\x6c\x65\x3e\x0d\x0a\x20"))
(assert (= x_12 (str.++ x_10 literal_11)))
(assert (str.in.re x_12 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_12 "\x61" 0)) (str.len x_12)))
(check-sat)
(get-model)
