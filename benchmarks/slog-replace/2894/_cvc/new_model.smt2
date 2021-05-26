(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun literal_3 () String)
(declare-fun x_4 () String)
(declare-fun literal_5 () String)
(declare-fun x_7 () String)
(declare-fun literal_9 () String)
(declare-fun x_10 () String)
(declare-fun literal_11 () String)
(declare-fun x_12 () String)
(declare-fun x_13 () String)
(declare-fun literal_14 () String)
(declare-fun x_15 () String)
(assert (= literal_3 "\x53\x45\x4c\x45\x43\x54\x20\x2a\x20\x46\x52\x4f\x4d\x20"))
(assert (= x_4 (str.++ literal_3 sigmaStar_2)))
(assert (= literal_5 "\x75\x73\x65\x72\x73\x20\x57\x48\x45\x52\x45\x20\x75\x73\x65\x72\x5f\x6c\x61\x73\x74\x76\x69\x73\x69\x74\x3e\x27\x30\x27\x20\x41\x4e\x44\x20\x75\x73\x65\x72\x5f\x73\x74\x61\x74\x75\x73\x3d\x27\x30\x27\x20\x4f\x52\x44\x45\x52\x20\x42\x59\x20\x75\x73\x65\x72\x5f\x6c\x61\x73\x74\x76\x69\x73\x69\x74\x20\x44\x45\x53\x43\x20\x4c\x49\x4d\x49\x54\x20\x30\x2c\x31\x30"))
(assert (= x_7 (str.++ x_4 literal_5)))
(assert (= literal_9 "\x3c\x61\x20\x68\x72\x65\x66\x3d\x27\x70\x72\x6f\x66\x69\x6c\x65\x2e\x70\x68\x70\x3f\x6c\x6f\x6f\x6b\x75\x70\x3d"))
(assert (= x_10 (str.++ literal_9 x_7)))
(assert (= literal_11 "\x27\x20\x74\x69\x74\x6c\x65\x3d\x27"))
(assert (= x_12 (str.++ x_10 literal_11)))
(assert (= x_13 (str.++ x_12 x_7)))
(assert (= literal_14 "\x27\x20\x63\x6c\x61\x73\x73\x3d\x27\x73\x69\x64\x65\x27\x3e\x5c\x6e"))
(assert (= x_15 (str.++ x_13 literal_14)))
(assert (str.in.re x_15 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_15 "\x61" 0)) (str.len x_15)))
(check-sat)
(get-model)