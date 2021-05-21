(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_2 () String)
(declare-fun x_7 () String)
(declare-fun literal_3 () String)
(declare-fun x_6 () String)
(declare-fun literal_4 () String)
(declare-fun literal_5 () String)
(declare-fun x_12 () String)
(declare-fun sigmaStar_13 () String)
(declare-fun x_8 () String)
(declare-fun epsilon () String)
(declare-fun literal_11 () String)
(declare-fun x_15 () String)
(declare-fun literal_16 () String)
(declare-fun x_17 () String)
(declare-fun x_18 () String)
(declare-fun x_14 () String)
(declare-fun x_20 () String)
(declare-fun literal_19 () String)
(declare-fun x_22 () String)
(declare-fun literal_21 () String)
(declare-fun x_23 () String)
(declare-fun literal_24 () String)
(declare-fun x_26 () String)
(declare-fun x_27 () String)
(declare-fun literal_25 () String)
(declare-fun x_28 () String)
(declare-fun x_32 () String)
(declare-fun literal_33 () String)
(declare-fun x_34 () String)
(declare-fun x_31 () String)
(declare-fun x_35 () String)
(declare-fun x_36 () String)
(assert (= literal_2 ""))
(assert (= x_7 (str.++ literal_2 sigmaStar_0)))
(assert (= literal_3 "\x3c\x74\x72\x3e\x0d\x0a\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27"))
(assert (= literal_4 "\x74\x62\x6c\x32"))
(assert (= literal_5 "\x74\x62\x6c\x31"))
(assert (or (= x_6 literal_4) (= x_6 literal_5)))
(assert (= x_12 (str.++ literal_3 x_6)))
(assert (= epsilon ""))
(assert (or (= x_8 epsilon)))
(assert (= x_14 (str.replaceall x_8 "\x2e" "\x20")))
(assert (= literal_11 ""))
(assert (= x_15 (str.++ x_7 literal_11)))
(assert (= literal_16 "\x3e\x3c\x73\x70\x61\x6e\x20\x74\x69\x74\x6c\x65\x3d"))
(assert (= x_17 (str.++ x_12 literal_16)))
(assert (or (= x_18 x_15) (= x_18 x_14)))
(assert (= x_20 (str.++ x_17 x_18)))
(assert (= literal_19 "\x3c\x2f\x73\x70\x61\x6e\x3e\x3c\x2f\x74\x64\x3e\x0d\x0a\x3c\x74\x64\x20\x61\x6c\x69\x67\x6e\x3d\x27\x63\x65\x6e\x74\x65\x72\x27\x20\x77\x69\x64\x74\x68\x3d\x27\x31\x25\x27\x20\x63\x6c\x61\x73\x73\x3d\x27"))
(assert (= x_22 (str.++ literal_19 x_6)))
(assert (= literal_21 "\x27\x20\x73\x74\x79\x6c\x65\x3d\x27\x63\x75\x72\x73\x6f\x72\x3a\x68\x61\x6e\x64\x3b\x27\x3e"))
(assert (= x_23 (str.++ x_20 literal_21)))
(assert (= literal_24 "\x20\x73\x74\x79\x6c\x65\x3d\x77\x68\x69\x74\x65\x2d\x73\x70\x61\x63\x65\x3a\x6e\x6f\x77\x72\x61\x70\x27\x3e"))
(assert (= x_26 (str.++ x_22 literal_24)))
(assert (= x_27 (str.++ x_23 x_8)))
(assert (= literal_25 "\x3c\x2f\x74\x64\x3e\x0d\x0a\x3c\x74\x64\x20\x61\x6c\x69\x67\x6e\x3d\x27\x63\x65\x6e\x74\x65\x72\x27\x20\x77\x69\x64\x74\x68\x3d\x27\x31\x25\x27\x20\x63\x6c\x61\x73\x73\x3d\x27"))
(assert (= x_28 (str.++ literal_25 x_6)))
(assert (= x_32 (str.++ x_27 x_26)))
(assert (= literal_33 "\x20\x73\x74\x79\x6c\x65\x3d\x77\x68\x69\x74\x65\x2d\x73\x70\x61\x63\x65\x3a\x6e\x6f\x77\x72\x61\x70\x27\x3e\x5c\x6e"))
(assert (= x_34 (str.++ x_28 literal_33)))
(assert (or (= x_31 epsilon)))
(assert (= x_35 (str.++ x_32 x_31)))
(assert (= x_36 (str.++ x_35 x_34)))
(assert (str.in.re x_36 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (> (* 2 (str.indexof x_14 "\x61" 0)) (str.len x_14)))
(check-sat)
(get-model)
