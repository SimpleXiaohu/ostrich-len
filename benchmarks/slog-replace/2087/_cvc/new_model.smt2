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
(declare-fun sigmaStar_7 () String)
(declare-fun literal_12 () String)
(declare-fun x_11 () String)
(declare-fun epsilon () String)
(declare-fun literal_9 () String)
(declare-fun x_13 () String)
(declare-fun literal_14 () String)
(declare-fun x_15 () String)
(declare-fun x_19 () String)
(declare-fun literal_17 () String)
(declare-fun x_22 () String)
(declare-fun literal_24 () String)
(declare-fun x_28 () String)
(declare-fun x_34 () String)
(declare-fun literal_30 () String)
(declare-fun x_37 () String)
(declare-fun literal_36 () String)
(declare-fun x_35 () String)
(declare-fun x_38 () String)
(declare-fun literal_42 () String)
(declare-fun x_41 () String)
(declare-fun x_46 () String)
(declare-fun literal_43 () String)
(declare-fun x_47 () String)
(declare-fun literal_45 () String)
(declare-fun x_54 () String)
(declare-fun literal_52 () String)
(declare-fun x_51 () String)
(declare-fun x_55 () String)
(declare-fun literal_53 () String)
(declare-fun x_56 () String)
(declare-fun literal_48 () String)
(declare-fun x_57 () String)
(declare-fun literal_58 () String)
(declare-fun x_62 () String)
(declare-fun literal_60 () String)
(declare-fun x_64 () String)
(declare-fun x_65 () String)
(declare-fun x_73 () String)
(declare-fun literal_72 () String)
(declare-fun x_78 () String)
(declare-fun literal_77 () String)
(declare-fun x_76 () String)
(declare-fun literal_74 () String)
(declare-fun x_80 () String)
(declare-fun literal_79 () String)
(declare-fun x_81 () String)
(declare-fun x_82 () String)
(declare-fun literal_25 () String)
(declare-fun x_83 () String)
(declare-fun literal_84 () String)
(declare-fun x_85 () String)
(declare-fun literal_86 () String)
(declare-fun x_87 () String)
(assert (= literal_12 "\x66\x72\x6f\x6d\x3d\x64\x61\x79\x26\x61\x6d\x70\x3b\x63\x61\x6c\x5f\x64\x3d"))
(assert (= epsilon ""))
(assert (= literal_9 "\x63\x61\x6c\x5f\x64"))
(assert (or (= x_11 epsilon) (= x_11 literal_9)))
(assert (= x_13 (str.++ literal_12 x_11)))
(assert (= literal_14 "\x26\x61\x6d\x70\x3b\x63\x61\x6c\x5f\x6d\x3d"))
(assert (= x_15 (str.++ x_13 literal_14)))
(assert (= literal_17 "\x63\x61\x6c\x5f\x6d"))
(assert (or (= x_19 epsilon) (= x_19 literal_17)))
(assert (= x_22 (str.++ x_15 x_19)))
(assert (= literal_24 "\x26\x61\x6d\x70\x3b\x63\x61\x6c\x5f\x79\x3d"))
(assert (= x_28 (str.++ x_22 literal_24)))
(assert (= literal_30 "\x63\x61\x6c\x5f\x79"))
(assert (or (= x_34 epsilon) (= x_34 literal_30)))
(assert (= x_37 (str.++ x_28 x_34)))
(assert (= literal_36 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x68\x69\x64\x64\x65\x6e\x22\x20\x6e\x61\x6d\x65\x3d\x22\x63\x61\x6c\x5f\x64\x22\x20\x76\x61\x6c\x75\x65\x3d\x22"))
(assert (or (= x_35 epsilon) (= x_35 literal_9)))
(assert (= x_38 (str.++ literal_36 x_35)))
(assert (= literal_42 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x68\x69\x64\x64\x65\x6e\x22\x20\x6e\x61\x6d\x65\x3d\x22\x63\x61\x6c\x5f\x6d\x22\x20\x76\x61\x6c\x75\x65\x3d\x22"))
(assert (or (= x_41 epsilon) (= x_41 literal_17)))
(assert (= x_46 (str.++ literal_42 x_41)))
(assert (= literal_43 "\x22\x20\x2f\x3e"))
(assert (= x_47 (str.++ x_38 literal_43)))
(assert (= literal_45 "\x2f\x63\x61\x6c\x65\x6e\x64\x61\x72\x2f\x73\x65\x74\x2e\x70\x68\x70\x3f\x76\x61\x72\x3d\x73\x65\x74\x63\x6f\x75\x72\x73\x65\x26\x61\x6d\x70\x3b"))
(assert (= x_54 (str.++ literal_45 x_37)))
(assert (= literal_52 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x68\x69\x64\x64\x65\x6e\x22\x20\x6e\x61\x6d\x65\x3d\x22\x63\x61\x6c\x5f\x79\x22\x20\x76\x61\x6c\x75\x65\x3d\x22"))
(assert (or (= x_51 epsilon) (= x_51 literal_30)))
(assert (= x_55 (str.++ literal_52 x_51)))
(assert (= literal_53 "\x22\x20\x2f\x3e"))
(assert (= x_56 (str.++ x_46 literal_53)))
(assert (= literal_48 "\x3c\x64\x69\x76\x20\x63\x6c\x61\x73\x73\x3d\x22\x62\x75\x74\x74\x6f\x6e\x73\x22\x3e\x3c\x66\x6f\x72\x6d\x20\x61\x63\x74\x69\x6f\x6e\x3d\x22\x2f\x63\x61\x6c\x65\x6e\x64\x61\x72\x2f\x65\x76\x65\x6e\x74\x2e\x70\x68\x70\x22\x20\x6d\x65\x74\x68\x6f\x64\x3d\x22\x67\x65\x74\x22\x3e\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x68\x69\x64\x64\x65\x6e\x22\x20\x6e\x61\x6d\x65\x3d\x22\x61\x63\x74\x69\x6f\x6e\x22\x20\x76\x61\x6c\x75\x65\x3d\x22\x6e\x65\x77\x22\x20\x2f\x3e"))
(assert (= x_57 (str.++ literal_48 x_47)))
(assert (= literal_58 "\x26\x61\x6d\x70\x3b\x69\x64\x3d"))
(assert (= x_62 (str.++ x_54 literal_58)))
(assert (= literal_60 "\x22\x20\x2f\x3e"))
(assert (= x_64 (str.++ x_55 literal_60)))
(assert (= x_65 (str.++ x_57 x_56)))
(assert (= x_73 (str.++ x_65 x_64)))
(assert (= literal_72 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x73\x75\x62\x6d\x69\x74\x22\x20\x76\x61\x6c\x75\x65\x3d\x22\x6e\x65\x77\x65\x76\x65\x6e\x74\x22\x20\x2f\x3e"))
(assert (= x_78 (str.++ x_73 literal_72)))
(assert (= literal_77 "\x64\x61\x79\x76\x69\x65\x77\x3a\x20"))
(assert (= literal_74 ""))
(assert (or (= x_76 literal_74) (= x_76 x_62)))
(assert (= x_80 (str.++ literal_77 x_76)))
(assert (= literal_79 "\x3c\x2f\x66\x6f\x72\x6d\x3e\x3c\x2f\x64\x69\x76\x3e"))
(assert (= x_81 (str.++ x_78 literal_79)))
(assert (= literal_25 ""))
(assert (or (= x_82 x_81) (= x_82 literal_25)))
(assert (= x_83 (str.++ x_82 x_80)))
(assert (= literal_84 "\x3c\x64\x69\x76\x20\x63\x6c\x61\x73\x73\x3d\x22\x68\x65\x61\x64\x65\x72\x22\x3e"))
(assert (= x_85 (str.++ literal_84 x_83)))
(assert (= literal_86 "\x3c\x2f\x64\x69\x76\x3e"))
(assert (= x_87 (str.++ x_85 literal_86)))
(assert (str.in.re x_87 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_87 "\x61" 0)) (str.len x_87)))
(check-sat)
(get-model)
