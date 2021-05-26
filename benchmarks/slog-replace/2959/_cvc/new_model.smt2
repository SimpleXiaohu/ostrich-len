(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_4 () String)
(declare-fun sigmaStar_5 () String)
(declare-fun sigmaStar_6 () String)
(declare-fun literal_7 () String)
(declare-fun x_8 () String)
(declare-fun literal_9 () String)
(declare-fun x_10 () String)
(declare-fun x_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_15 () String)
(declare-fun x_17 () String)
(declare-fun literal_19 () String)
(declare-fun x_21 () String)
(declare-fun literal_18 () String)
(declare-fun x_24 () String)
(declare-fun literal_23 () String)
(declare-fun x_25 () String)
(declare-fun literal_22 () String)
(declare-fun x_26 () String)
(declare-fun literal_27 () String)
(declare-fun x_29 () String)
(declare-fun x_31 () String)
(declare-fun literal_30 () String)
(declare-fun x_33 () String)
(declare-fun literal_32 () String)
(declare-fun x_34 () String)
(declare-fun literal_35 () String)
(declare-fun x_36 () String)
(declare-fun x_37 () String)
(declare-fun x_39 () String)
(declare-fun literal_40 () String)
(declare-fun x_41 () String)
(assert (= literal_7 "\x0d\x0a\x09\x53\x45\x4c\x45\x43\x54\x20\x74\x66\x2e\x66\x6f\x72\x75\x6d\x5f\x69\x64\x2c\x20\x74\x74\x2e\x74\x68\x72\x65\x61\x64\x5f\x69\x64\x2c\x20\x74\x74\x2e\x74\x68\x72\x65\x61\x64\x5f\x73\x75\x62\x6a\x65\x63\x74\x2c\x20\x43\x4f\x55\x4e\x54\x28\x74\x70\x2e\x70\x6f\x73\x74\x5f\x69\x64\x29\x20\x61\x73\x20\x63\x6f\x75\x6e\x74\x5f\x70\x6f\x73\x74\x73\x20\x0d\x0a\x09\x46\x52\x4f\x4d\x20"))
(assert (= x_8 (str.++ literal_7 sigmaStar_2)))
(assert (= literal_9 "\x66\x6f\x72\x75\x6d\x73\x20\x74\x66\x0d\x0a\x09\x49\x4e\x4e\x45\x52\x20\x4a\x4f\x49\x4e\x20"))
(assert (= x_10 (str.++ x_8 literal_9)))
(assert (= x_12 (str.++ x_10 sigmaStar_1)))
(assert (= literal_13 "\x74\x68\x72\x65\x61\x64\x73\x20\x74\x74\x20\x55\x53\x49\x4e\x47\x28\x66\x6f\x72\x75\x6d\x5f\x69\x64\x29\x0d\x0a\x09\x49\x4e\x4e\x45\x52\x20\x4a\x4f\x49\x4e\x20"))
(assert (= x_15 (str.++ x_12 literal_13)))
(assert (= x_17 (str.++ x_15 sigmaStar_6)))
(assert (= literal_19 "\x70\x6f\x73\x74\x73\x20\x74\x70\x20\x55\x53\x49\x4e\x47\x28\x74\x68\x72\x65\x61\x64\x5f\x69\x64\x29\x0d\x0a\x09\x57\x48\x45\x52\x45\x20"))
(assert (= x_21 (str.++ x_17 literal_19)))
(assert (= literal_18 "\x66\x6f\x72\x75\x6d\x5f\x61\x63\x63\x65\x73\x73"))
(assert (= x_24 (str.++ x_21 literal_18)))
(assert (= literal_23 "\x20\x47\x52\x4f\x55\x50\x20\x42\x59\x20\x74\x68\x72\x65\x61\x64\x5f\x69\x64\x20\x4f\x52\x44\x45\x52\x20\x42\x59\x20\x63\x6f\x75\x6e\x74\x5f\x70\x6f\x73\x74\x73\x20\x44\x45\x53\x43\x2c\x20\x74\x68\x72\x65\x61\x64\x5f\x6c\x61\x73\x74\x70\x6f\x73\x74\x20\x44\x45\x53\x43\x20\x4c\x49\x4d\x49\x54\x20\x35\x0d\x0a"))
(assert (= x_25 (str.++ x_24 literal_23)))
(assert (= literal_22 "\x3c\x74\x72\x3e\x5c\x6e\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27\x73\x69\x64\x65\x2d\x73\x6d\x61\x6c\x6c\x27\x3e\x3c\x69\x6d\x67\x20\x73\x72\x63\x3d\x27\x69\x6d\x61\x67\x65\x73\x2f\x62\x75\x6c\x6c\x65\x74\x2e\x67\x69\x66\x27\x20\x61\x6c\x74\x3d\x27\x27\x3e\x20\x3c\x61\x20\x68\x72\x65\x66\x3d\x27\x76\x69\x65\x77\x74\x68\x72\x65\x61\x64\x2e\x70\x68\x70\x3f\x66\x6f\x72\x75\x6d\x5f\x69\x64\x3d"))
(assert (= x_26 (str.++ literal_22 x_25)))
(assert (= literal_27 "\x26\x61\x6d\x70\x3b\x74\x68\x72\x65\x61\x64\x5f\x69\x64\x3d"))
(assert (= x_29 (str.++ x_26 literal_27)))
(assert (= x_31 (str.++ x_29 x_25)))
(assert (= literal_30 "\x20\x63\x6c\x61\x73\x73\x3d\x73\x69\x64\x65\x27\x3e"))
(assert (= x_33 (str.++ literal_30 x_25)))
(assert (= literal_32 "\x27\x20\x74\x69\x74\x6c\x65\x3d\x27"))
(assert (= x_34 (str.++ x_31 literal_32)))
(assert (= literal_35 "\x3c\x2f\x61\x3e\x3c\x2f\x74\x64\x3e\x0d\x0a\x3c\x74\x64\x20\x61\x6c\x69\x67\x6e\x3d\x27\x72\x69\x67\x68\x74\x27\x20\x63\x6c\x61\x73\x73\x3d\x27\x73\x69\x64\x65\x2d\x73\x6d\x61\x6c\x6c\x27\x3e\x5b"))
(assert (= x_36 (str.++ x_33 literal_35)))
(assert (= x_37 (str.++ x_34 x_25)))
(assert (= x_39 (str.++ x_37 x_36)))
(assert (= literal_40 "\x5d\x3c\x2f\x74\x64\x3e\x5c\x6e\x3c\x2f\x74\x72\x3e\x5c\x6e"))
(assert (= x_41 (str.++ x_39 literal_40)))
(assert (str.in.re x_41 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_41 "\x61" 0)) (str.len x_41)))
(check-sat)
(get-model)