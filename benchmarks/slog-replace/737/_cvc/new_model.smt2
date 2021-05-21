(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun literal_6 () String)
(declare-fun x_7 () String)
(declare-fun epsilon () String)
(declare-fun literal_5 () String)
(declare-fun x_8 () String)
(declare-fun literal_10 () String)
(declare-fun x_11 () String)
(declare-fun literal_9 () String)
(declare-fun x_14 () String)
(declare-fun literal_16 () String)
(declare-fun x_17 () String)
(assert (= literal_6 "\x2f\x65\x6e\x72\x6f\x6c\x2f\x61\x75\x74\x68\x6f\x72\x69\x7a\x65\x2f\x69\x6e\x64\x65\x78\x2e\x70\x68\x70\x3f\x75\x73\x65\x72\x3d"))
(assert (= epsilon ""))
(assert (= literal_5 "\x75\x73\x65\x72"))
(assert (or (= x_7 epsilon) (= x_7 literal_5)))
(assert (= x_8 (str.++ literal_6 x_7)))
(assert (= literal_10 "\x26\x61\x6d\x70\x3b\x63\x6f\x75\x72\x73\x65\x3d"))
(assert (= x_11 (str.++ x_8 literal_10)))
(assert (= literal_9 "\x63\x6f\x75\x72\x73\x65"))
(assert (= x_14 (str.++ x_11 literal_9)))
(assert (= literal_16 "\x26\x61\x6d\x70\x3b\x73\x74\x61\x74\x75\x73\x3d"))
(assert (= x_17 (str.++ x_14 literal_16)))
(assert (str.in.re x_17 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_17 "\x61" 0)) (str.len x_17)))
(check-sat)
(get-model)
