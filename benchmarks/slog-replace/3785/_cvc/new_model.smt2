(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_4 () String)
(declare-fun sigmaStar_5 () String)
(declare-fun literal_6 () String)
(declare-fun x_7 () String)
(declare-fun literal_8 () String)
(declare-fun x_11 () String)
(declare-fun x_12 () String)
(declare-fun epsilon () String)
(declare-fun x_13 () String)
(declare-fun literal_14 () String)
(declare-fun x_15 () String)
(declare-fun x_16 () String)
(declare-fun literal_17 () String)
(declare-fun x_18 () String)
(declare-fun x_19 () String)
(declare-fun x_20 () String)
(declare-fun literal_21 () String)
(declare-fun x_22 () String)
(declare-fun x_23 () String)
(declare-fun literal_24 () String)
(declare-fun x_25 () String)
(declare-fun x_26 () String)
(declare-fun x_27 () String)
(declare-fun literal_28 () String)
(declare-fun x_29 () String)
(assert (= literal_6 "\x3c\x2f\x74\x64\x3e\x0d\x0a\x3c\x2f\x74\x72\x3e\x0d\x0a\x3c\x74\x72\x3e\x0d\x0a\x3c\x74\x64\x20\x77\x69\x64\x74\x68\x3d\x27\x31\x25\x27\x20\x63\x6c\x61\x73\x73\x3d\x27\x74\x62\x6c\x31\x27\x20\x73\x74\x79\x6c\x65\x3d\x27\x77\x68\x69\x74\x65\x2d\x73\x70\x61\x63\x65\x3a\x6e\x6f\x77\x72\x61\x70\x27\x3e\x3c\x62\x3e"))
(assert (= x_7 (str.++ literal_6 sigmaStar_2)))
(assert (= literal_8 "\x3c\x2f\x62\x3e\x3c\x2f\x74\x64\x3e\x0d\x0a\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27\x74\x62\x6c\x31\x27\x3e"))
(assert (= x_11 (str.++ x_7 literal_8)))
(assert (= epsilon ""))
(assert (or (= x_12 epsilon) (= x_12 sigmaStar_5)))
(assert (= x_13 (str.++ x_11 x_12)))
(assert (= literal_14 "\x3c\x2f\x74\x64\x3e\x0d\x0a\x3c\x2f\x74\x72\x3e\x0d\x0a\x3c\x74\x72\x3e\x0d\x0a\x3c\x74\x64\x20\x77\x69\x64\x74\x68\x3d\x27\x31\x25\x27\x20\x63\x6c\x61\x73\x73\x3d\x27\x74\x62\x6c\x32\x27\x20\x73\x74\x79\x6c\x65\x3d\x27\x77\x68\x69\x74\x65\x2d\x73\x70\x61\x63\x65\x3a\x6e\x6f\x77\x72\x61\x70\x27\x3e\x3c\x62\x3e"))
(assert (= x_15 (str.++ x_13 literal_14)))
(assert (= x_16 (str.++ x_15 sigmaStar_1)))
(assert (= literal_17 "\x3c\x2f\x62\x3e\x3c\x2f\x74\x64\x3e\x0d\x0a\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27\x74\x62\x6c\x32\x27\x3e"))
(assert (= x_18 (str.++ x_16 literal_17)))
(assert (or (= x_19 epsilon) (= x_19 sigmaStar_0)))
(assert (= x_20 (str.++ x_18 x_19)))
(assert (= literal_21 "\x3c\x2f\x74\x64\x3e\x0d\x0a\x3c\x2f\x74\x72\x3e\x0d\x0a\x3c\x74\x72\x3e\x0d\x0a\x3c\x74\x64\x20\x77\x69\x64\x74\x68\x3d\x27\x31\x25\x27\x20\x63\x6c\x61\x73\x73\x3d\x27\x74\x62\x6c\x31\x27\x20\x73\x74\x79\x6c\x65\x3d\x27\x77\x68\x69\x74\x65\x2d\x73\x70\x61\x63\x65\x3a\x6e\x6f\x77\x72\x61\x70\x27\x3e\x3c\x62\x3e"))
(assert (= x_22 (str.++ x_20 literal_21)))
(assert (= x_23 (str.++ x_22 sigmaStar_4)))
(assert (= literal_24 "\x3c\x2f\x62\x3e\x3c\x2f\x74\x64\x3e\x0d\x0a\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27\x74\x62\x6c\x31\x27\x3e"))
(assert (= x_25 (str.++ x_23 literal_24)))
(assert (or (= x_26 epsilon) (= x_26 sigmaStar_3)))
(assert (= x_27 (str.++ x_25 x_26)))
(assert (= literal_28 "\x3c\x2f\x74\x64\x3e\x0d\x0a\x3c\x2f\x74\x72\x3e\x0d\x0a\x3c\x74\x72\x3e\x0d\x0a\x3c\x74\x64\x20\x61\x6c\x69\x67\x6e\x3d\x27\x63\x65\x6e\x74\x65\x72\x27\x20\x63\x6c\x61\x73\x73\x3d\x27\x74\x62\x6c\x31\x27\x3e\x5c\x6e"))
(assert (= x_29 (str.++ x_27 literal_28)))
(assert (str.in.re x_29 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_29 "\x61" 0)) (str.len x_29)))
(check-sat)
(get-model)
