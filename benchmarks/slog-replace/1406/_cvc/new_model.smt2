(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_4 () String)
(declare-fun literal_9 () String)
(declare-fun x_10 () String)
(declare-fun literal_11 () String)
(declare-fun x_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_14 () String)
(declare-fun literal_15 () String)
(declare-fun x_17 () String)
(declare-fun literal_18 () String)
(declare-fun x_19 () String)
(declare-fun literal_16 () String)
(declare-fun x_20 () String)
(declare-fun literal_21 () String)
(declare-fun x_22 () String)
(declare-fun literal_23 () String)
(declare-fun x_24 () String)
(declare-fun x_29 () String)
(declare-fun literal_27 () String)
(declare-fun literal_28 () String)
(declare-fun x_30 () String)
(declare-fun literal_32 () String)
(declare-fun x_33 () String)
(declare-fun literal_31 () String)
(declare-fun x_34 () String)
(declare-fun literal_35 () String)
(declare-fun x_36 () String)
(declare-fun literal_37 () String)
(declare-fun x_38 () String)
(declare-fun literal_39 () String)
(declare-fun x_40 () String)
(declare-fun literal_41 () String)
(declare-fun x_42 () String)
(declare-fun x_43 () String)
(declare-fun literal_44 () String)
(declare-fun x_45 () String)
(declare-fun literal_46 () String)
(declare-fun x_47 () String)
(declare-fun literal_48 () String)
(declare-fun x_49 () String)
(declare-fun x_50 () String)
(declare-fun x_51 () String)
(declare-fun literal_53 () String)
(declare-fun x_54 () String)
(declare-fun literal_52 () String)
(declare-fun x_55 () String)
(declare-fun literal_56 () String)
(declare-fun x_57 () String)
(declare-fun literal_58 () String)
(declare-fun x_59 () String)
(declare-fun literal_60 () String)
(declare-fun x_61 () String)
(assert (= literal_9 "\x3c\x74\x61\x62\x6c\x65\x20\x62\x6f\x72\x64\x65\x72\x3d\x22\x30\x22\x3e\x3c\x74\x72\x3e\x3c\x74\x64\x3e\x3c\x66\x6f\x72\x6d\x20\x74\x61\x72\x67\x65\x74\x3d\x22\x5f\x70\x61\x72\x65\x6e\x74\x22\x20\x6d\x65\x74\x68\x6f\x64\x3d\x22\x70\x6f\x73\x74\x22\x20\x61\x63\x74\x69\x6f\x6e\x3d\x22"))
(assert (= x_10 (str.++ literal_9 sigmaStar_2)))
(assert (= literal_11 "\x22\x3e"))
(assert (= x_12 (str.++ x_10 literal_11)))
(assert (= literal_13 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x68\x69\x64\x64\x65\x6e\x22\x20\x6e\x61\x6d\x65\x3d\x22\x69\x64\x22\x20\x76\x61\x6c\x75\x65\x3d\x22"))
(assert (= x_14 (str.++ x_12 literal_13)))
(assert (= literal_15 "\x22\x3e"))
(assert (= x_17 (str.++ x_14 literal_15)))
(assert (= literal_18 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x68\x69\x64\x64\x65\x6e\x22\x20\x6e\x61\x6d\x65\x3d\x22\x72\x65\x67\x72\x61\x64\x65\x22\x20\x76\x61\x6c\x75\x65\x3d\x22"))
(assert (= x_19 (str.++ x_17 literal_18)))
(assert (= literal_16 "\x72\x65\x67\x72\x61\x64\x65"))
(assert (= x_20 (str.++ x_19 literal_16)))
(assert (= literal_21 "\x22\x20\x2f\x3e"))
(assert (= x_22 (str.++ x_20 literal_21)))
(assert (= literal_23 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x68\x69\x64\x64\x65\x6e\x22\x20\x6e\x61\x6d\x65\x3d\x22\x63\x6f\x6e\x66\x69\x72\x6d\x22\x20\x76\x61\x6c\x75\x65\x3d\x22\x31\x22\x20\x2f\x3e"))
(assert (= x_24 (str.++ x_22 literal_23)))
(assert (= literal_27 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x68\x69\x64\x64\x65\x6e\x22\x20\x6e\x61\x6d\x65\x3d\x22\x73\x65\x73\x73\x6b\x65\x79\x22\x20\x76\x61\x6c\x75\x65\x3d\x22\x22\x20\x2f\x3e"))
(assert (= literal_28 ""))
(assert (or (= x_29 literal_27) (= x_29 literal_28)))
(assert (= x_30 (str.++ x_24 x_29)))
(assert (= literal_32 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x73\x75\x62\x6d\x69\x74\x22\x20\x76\x61\x6c\x75\x65\x3d\x22"))
(assert (= x_33 (str.++ x_30 literal_32)))
(assert (= literal_31 "\x79\x65\x73"))
(assert (= x_34 (str.++ x_33 literal_31)))
(assert (= literal_35 "\x22\x20\x2f\x3e"))
(assert (= x_36 (str.++ x_34 literal_35)))
(assert (= literal_37 "\x3c\x2f\x66\x6f\x72\x6d\x3e"))
(assert (= x_38 (str.++ x_36 literal_37)))
(assert (= literal_39 "\x3c\x2f\x74\x64\x3e\x3c\x74\x64\x3e\x20\x26\x6e\x62\x73\x70\x3b\x20\x3c\x2f\x74\x64\x3e\x3c\x74\x64\x3e"))
(assert (= x_40 (str.++ x_38 literal_39)))
(assert (= literal_41 "\x3c\x66\x6f\x72\x6d\x20\x74\x61\x72\x67\x65\x74\x3d\x22\x5f\x70\x61\x72\x65\x6e\x74\x22\x20\x6d\x65\x74\x68\x6f\x64\x3d\x22\x70\x6f\x73\x74\x22\x20\x61\x63\x74\x69\x6f\x6e\x3d\x22"))
(assert (= x_42 (str.++ x_40 literal_41)))
(assert (= x_43 (str.++ x_42 sigmaStar_3)))
(assert (= literal_44 "\x22\x3e"))
(assert (= x_45 (str.++ x_43 literal_44)))
(assert (= literal_46 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x68\x69\x64\x64\x65\x6e\x22\x20\x6e\x61\x6d\x65\x3d\x22\x69\x64\x22\x20\x76\x61\x6c\x75\x65\x3d\x22"))
(assert (= x_47 (str.++ x_45 literal_46)))
(assert (= literal_48 "\x22\x3e"))
(assert (= x_49 (str.++ x_47 literal_48)))
(assert (or (= x_50 literal_27) (= x_50 literal_28)))
(assert (= x_51 (str.++ x_49 x_50)))
(assert (= literal_53 "\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x22\x73\x75\x62\x6d\x69\x74\x22\x20\x76\x61\x6c\x75\x65\x3d\x22"))
(assert (= x_54 (str.++ x_51 literal_53)))
(assert (= literal_52 "\x6e\x6f"))
(assert (= x_55 (str.++ x_54 literal_52)))
(assert (= literal_56 "\x22\x20\x2f\x3e"))
(assert (= x_57 (str.++ x_55 literal_56)))
(assert (= literal_58 "\x3c\x2f\x66\x6f\x72\x6d\x3e"))
(assert (= x_59 (str.++ x_57 literal_58)))
(assert (= literal_60 "\x3c\x2f\x74\x64\x3e\x3c\x2f\x74\x72\x3e\x3c\x2f\x74\x61\x62\x6c\x65\x3e"))
(assert (= x_61 (str.++ x_59 literal_60)))
(assert (str.in.re x_61 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_61 "\x61" 0)) (str.len x_61)))
(check-sat)
(get-model)
