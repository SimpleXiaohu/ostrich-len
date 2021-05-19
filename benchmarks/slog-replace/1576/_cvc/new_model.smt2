(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun literal_11 () String)
(declare-fun x_14 () String)
(declare-fun literal_8 () String)
(declare-fun literal_10 () String)
(declare-fun x_19 () String)
(declare-fun literal_22 () String)
(declare-fun x_24 () String)
(declare-fun literal_21 () String)
(declare-fun x_20 () String)
(declare-fun epsilon () String)
(declare-fun literal_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_25 () String)
(declare-fun x_23 () String)
(declare-fun x_27 () String)
(declare-fun literal_26 () String)
(declare-fun x_28 () String)
(declare-fun literal_29 () String)
(declare-fun x_31 () String)
(declare-fun x_32 () String)
(declare-fun literal_30 () String)
(declare-fun x_33 () String)
(declare-fun literal_34 () String)
(declare-fun x_35 () String)
(declare-fun literal_45 () String)
(assert (= literal_11 "\x53\x45\x4c\x45\x43\x54\x20\x4d\x41\x58\x28\x61\x2e\x74\x69\x6d\x65\x29\x20\x61\x73\x20\x74\x69\x6d\x65\x2c\x20\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x75\x2e\x69\x64\x2c\x20\x75\x2e\x66\x69\x72\x73\x74\x6e\x61\x6d\x65\x2c\x20\x75\x2e\x6c\x61\x73\x74\x6e\x61\x6d\x65\x2c\x20\x75\x2e\x70\x69\x63\x74\x75\x72\x65\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x46\x52\x4f\x4d\x20\x73\x75\x72\x76\x65\x79\x5f\x61\x6e\x73\x77\x65\x72\x73\x20\x41\x53\x20\x61\x2c\x20\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x75\x73\x65\x72\x20\x41\x53\x20\x75\x20\x20\x20"))
(assert (= literal_8 "\x2c\x20\x67\x72\x6f\x75\x70\x73\x5f\x6d\x65\x6d\x62\x65\x72\x73\x20\x41\x53\x20\x67\x6d"))
(assert (= literal_10 ""))
(assert (or (= x_14 literal_8) (= x_14 literal_10)))
(assert (= x_19 (str.++ literal_11 x_14)))
(assert (= literal_22 "\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x57\x48\x45\x52\x45\x20\x61\x2e\x73\x75\x72\x76\x65\x79\x20\x3d\x20"))
(assert (= x_24 (str.++ x_19 literal_22)))
(assert (= literal_21 "\x41\x4e\x44\x20\x67\x6d\x2e\x67\x72\x6f\x75\x70\x69\x64\x20\x3d\x20"))
(assert (= epsilon ""))
(assert (= literal_12 "\x30"))
(assert (= literal_13 "\x30"))
(assert (or (= x_20 epsilon) (= x_20 literal_12) (= x_20 literal_13)))
(assert (= x_25 (str.++ literal_21 x_20)))
(assert (or (= x_23 epsilon)))
(assert (= x_27 (str.++ x_24 x_23)))
(assert (= literal_26 "\x20\x41\x4e\x44\x20\x75\x2e\x69\x64\x20\x3d\x20\x67\x6d\x2e\x75\x73\x65\x72\x69\x64"))
(assert (= x_28 (str.++ x_25 literal_26)))
(assert (= literal_29 "\x20\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x41\x4e\x44\x20\x61\x2e\x75\x73\x65\x72\x69\x64\x20\x3d\x20\x75\x2e\x69\x64\x20"))
(assert (= x_31 (str.++ x_27 literal_29)))
(assert (= literal_30 ""))
(assert (or (= x_32 literal_30) (= x_32 x_28)))
(assert (= x_33 (str.++ x_31 x_32)))
(assert (= literal_34 "\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x47\x52\x4f\x55\x50\x20\x42\x59\x20\x75\x2e\x69\x64\x2c\x20\x75\x2e\x66\x69\x72\x73\x74\x6e\x61\x6d\x65\x2c\x20\x75\x2e\x6c\x61\x73\x74\x6e\x61\x6d\x65\x2c\x20\x75\x2e\x70\x69\x63\x74\x75\x72\x65\x0d\x0a\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x4f\x52\x44\x45\x52\x20\x42\x59\x20\x74\x69\x6d\x65\x20\x41\x53\x43"))
(assert (= x_35 (str.++ x_33 literal_34)))
(assert (= literal_45 "\x3c\x64\x69\x76\x20\x63\x6c\x61\x73\x73\x3d\x5c\x22\x72\x65\x70\x6f\x72\x74\x6c\x69\x6e\x6b\x5c\x22\x3e\x3c\x61\x20\x68\x72\x65\x66\x3d\x5c\x22\x72\x65\x70\x6f\x72\x74\x2e\x70\x68\x70\x3f\x69\x64\x3d\x5c\x22\x3e\x76\x69\x65\x77\x73\x75\x72\x76\x65\x79\x72\x65\x73\x70\x6f\x6e\x73\x65\x73\x3c\x2f\x61\x3e\x3c\x2f\x64\x69\x76\x3e"))
(assert (str.in.re literal_45 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_35 "\x61" 0)) (str.len x_35)))
(check-sat)
(get-model)
