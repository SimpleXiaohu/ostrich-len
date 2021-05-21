(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_4 () String)
(declare-fun sigmaStar_6 () String)
(declare-fun literal_17 () String)
(declare-fun x_16 () String)
(declare-fun literal_8 () String)
(declare-fun literal_12 () String)
(declare-fun literal_5 () String)
(declare-fun x_20 () String)
(declare-fun literal_23 () String)
(declare-fun x_27 () String)
(declare-fun x_31 () String)
(declare-fun epsilon () String)
(declare-fun literal_21 () String)
(declare-fun literal_22 () String)
(declare-fun literal_13 () String)
(declare-fun literal_14 () String)
(declare-fun x_32 () String)
(declare-fun literal_33 () String)
(declare-fun x_34 () String)
(assert (= literal_17 "\x3c\x74\x72\x3e\x3c\x74\x68\x20\x61\x6c\x69\x67\x6e\x3d\x22\x72\x69\x67\x68\x74\x22\x20\x77\x69\x64\x74\x68\x3d\x22\x31\x30\x30\x22\x20\x63\x6c\x61\x73\x73\x3d\x22\x67\x65\x6e\x65\x72\x61\x6c\x74\x61\x62\x6c\x65\x68\x65\x61\x64\x65\x72\x22\x3e"))
(assert (= literal_8 "\x72\x65\x70\x6f\x72\x74"))
(assert (= literal_12 "\x70\x65\x6e\x61\x6c\x74\x69\x65\x73"))
(assert (= literal_5 ""))
(assert (or (= x_16 literal_8) (= x_16 literal_12) (= x_16 literal_5)))
(assert (= x_20 (str.++ literal_17 x_16)))
(assert (= literal_23 "\x3a\x3c\x2f\x74\x68\x3e\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x22\x67\x65\x6e\x65\x72\x61\x6c\x74\x61\x62\x6c\x65\x63\x65\x6c\x6c\x22\x3e"))
(assert (= x_27 (str.++ x_20 literal_23)))
(assert (= epsilon ""))
(assert (= literal_21 "\x2d"))
(assert (= literal_22 "\x2d"))
(assert (= literal_13 "\x26\x6e\x62\x73\x70\x3b"))
(assert (= literal_14 "\x26\x6e\x62\x73\x70\x3b"))
(assert (or (= x_31 epsilon) (= x_31 literal_21) (= x_31 literal_22) (= x_31 literal_13) (= x_31 literal_14)))
(assert (= x_32 (str.++ x_27 x_31)))
(assert (= literal_33 "\x3c\x2f\x74\x64\x3e\x3c\x2f\x74\x72\x3e"))
(assert (= x_34 (str.++ x_32 literal_33)))
(assert (str.in.re x_34 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_34 "\x61" 0)) (str.len x_34)))
(check-sat)
(get-model)
