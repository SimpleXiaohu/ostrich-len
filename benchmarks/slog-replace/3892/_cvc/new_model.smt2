(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun literal_7 () String)
(declare-fun x_8 () String)
(declare-fun x_9 () String)
(declare-fun literal_10 () String)
(declare-fun x_11 () String)
(declare-fun x_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_14 () String)
(assert (= literal_7 "\x20\x3c\x73\x65\x6c\x65\x63\x74\x20\x6e\x61\x6d\x65\x3d\x27\x6e\x75\x6d\x5f\x64\x61\x79\x73\x27\x20\x63\x6c\x61\x73\x73\x3d\x27\x74\x65\x78\x74\x62\x6f\x78\x27\x20\x73\x74\x79\x6c\x65\x3d\x27\x77\x69\x64\x74\x68\x3a\x35\x30\x70\x78\x27\x3e\x3c\x6f\x70\x74\x69\x6f\x6e\x20\x76\x61\x6c\x75\x65\x3d\x27\x39\x30\x27\x3e\x39\x30\x3c\x2f\x6f\x70\x74\x69\x6f\x6e\x3e\x0d\x0a\x3c\x6f\x70\x74\x69\x6f\x6e\x20\x76\x61\x6c\x75\x65\x3d\x27\x36\x30\x27\x3e\x36\x30\x3c\x2f\x6f\x70\x74\x69\x6f\x6e\x3e\x0d\x0a\x3c\x6f\x70\x74\x69\x6f\x6e\x20\x76\x61\x6c\x75\x65\x3d\x27\x33\x30\x27\x3e\x33\x30\x3c\x2f\x6f\x70\x74\x69\x6f\x6e\x3e\x0d\x0a\x3c\x6f\x70\x74\x69\x6f\x6e\x20\x76\x61\x6c\x75\x65\x3d\x27\x32\x30\x27\x3e\x32\x30\x3c\x2f\x6f\x70\x74\x69\x6f\x6e\x3e\x0d\x0a\x3c\x6f\x70\x74\x69\x6f\x6e\x20\x76\x61\x6c\x75\x65\x3d\x27\x31\x30\x27\x3e\x31\x30\x3c\x2f\x6f\x70\x74\x69\x6f\x6e\x3e\x5c\x6e\x3c\x2f\x73\x65\x6c\x65\x63\x74\x3e"))
(assert (= x_8 (str.++ sigmaStar_1 literal_7)))
(assert (= x_9 (str.++ x_8 sigmaStar_2)))
(assert (= literal_10 "\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x0d\x0a\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x27\x73\x75\x62\x6d\x69\x74\x27\x20\x6e\x61\x6d\x65\x3d\x27\x64\x65\x6c\x65\x74\x65\x73\x68\x6f\x75\x74\x73\x27\x20\x76\x61\x6c\x75\x65\x3d\x27"))
(assert (= x_11 (str.++ x_9 literal_10)))
(assert (= x_12 (str.++ x_11 sigmaStar_0)))
(assert (= literal_13 "\x27\x20\x63\x6c\x61\x73\x73\x3d\x27\x62\x75\x74\x74\x6f\x6e\x27\x3e\x3c\x62\x72\x3e\x3c\x68\x72\x3e\x0d\x0a\x3c\x2f\x64\x69\x76\x3e\x0d\x0a\x3c\x2f\x66\x6f\x72\x6d\x3e\x5c\x6e"))
(assert (= x_14 (str.++ x_12 literal_13)))
(assert (str.in.re x_14 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_14 "\x61" 0)) (str.len x_14)))
(check-sat)
(get-model)
