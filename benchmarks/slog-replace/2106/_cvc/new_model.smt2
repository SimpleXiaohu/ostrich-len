(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_4 () String)
(declare-fun sigmaStar_5 () String)
(declare-fun literal_9 () String)
(declare-fun x_8 () String)
(declare-fun epsilon () String)
(declare-fun literal_7 () String)
(declare-fun x_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_14 () String)
(declare-fun x_17 () String)
(declare-fun literal_11 () String)
(declare-fun x_21 () String)
(declare-fun literal_23 () String)
(declare-fun x_25 () String)
(declare-fun x_26 () String)
(declare-fun literal_20 () String)
(declare-fun x_28 () String)
(declare-fun literal_30 () String)
(declare-fun x_31 () String)
(declare-fun literal_34 () String)
(declare-fun x_35 () String)
(declare-fun literal_33 () String)
(declare-fun x_36 () String)
(declare-fun literal_37 () String)
(declare-fun x_38 () String)
(declare-fun literal_39 () String)
(declare-fun x_40 () String)
(assert (= literal_9 "\x66\x72\x6f\x6d\x3d\x6d\x6f\x6e\x74\x68\x26\x61\x6d\x70\x3b\x63\x61\x6c\x5f\x64\x3d"))
(assert (= epsilon ""))
(assert (= literal_7 "\x63\x61\x6c\x5f\x64"))
(assert (or (= x_8 epsilon) (= x_8 literal_7)))
(assert (= x_12 (str.++ literal_9 x_8)))
(assert (= literal_13 "\x26\x61\x6d\x70\x3b\x63\x61\x6c\x5f\x6d\x3d"))
(assert (= x_14 (str.++ x_12 literal_13)))
(assert (= literal_11 "\x63\x61\x6c\x5f\x6d"))
(assert (or (= x_17 epsilon) (= x_17 literal_11)))
(assert (= x_21 (str.++ x_14 x_17)))
(assert (= literal_23 "\x26\x61\x6d\x70\x3b\x63\x61\x6c\x5f\x79\x3d"))
(assert (= x_25 (str.++ x_21 literal_23)))
(assert (= literal_20 "\x63\x61\x6c\x5f\x79"))
(assert (or (= x_26 epsilon) (= x_26 literal_20)))
(assert (= x_28 (str.++ x_25 x_26)))
(assert (= literal_30 "\x68\x69\x64\x64\x65\x6e\x20\x28\x3c\x61\x20\x68\x72\x65\x66\x3d\x22\x2f\x63\x61\x6c\x65\x6e\x64\x61\x72\x2f\x73\x65\x74\x2e\x70\x68\x70\x3f\x76\x61\x72\x3d\x73\x68\x6f\x77\x63\x6f\x75\x72\x73\x65\x73\x26\x61\x6d\x70\x3b"))
(assert (= x_31 (str.++ literal_30 x_28)))
(assert (= literal_34 "\x22\x3e"))
(assert (= x_35 (str.++ x_31 literal_34)))
(assert (= literal_33 "\x63\x6c\x69\x63\x6b\x73\x68\x6f\x77"))
(assert (= x_36 (str.++ x_35 literal_33)))
(assert (= literal_37 "\x3c\x2f\x61\x3e\x29\x3c\x2f\x74\x64\x3e"))
(assert (= x_38 (str.++ x_36 literal_37)))
(assert (= literal_39 "\x5c\x6e"))
(assert (= x_40 (str.++ x_38 literal_39)))
(assert (str.in.re x_40 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_40 "\x61" 0)) (str.len x_40)))
(check-sat)
(get-model)