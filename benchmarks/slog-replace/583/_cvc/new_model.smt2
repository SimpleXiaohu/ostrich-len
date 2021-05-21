(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_10 () String)
(declare-fun sigmaStar_13 () String)
(declare-fun sigmaStar_14 () String)
(declare-fun sigmaStar_16 () String)
(declare-fun sigmaStar_18 () String)
(declare-fun sigmaStar_20 () String)
(declare-fun sigmaStar_24 () String)
(declare-fun sigmaStar_25 () String)
(declare-fun literal_27 () String)
(declare-fun x_28 () String)
(declare-fun literal_29 () String)
(declare-fun x_30 () String)
(declare-fun literal_33 () String)
(declare-fun x_35 () String)
(declare-fun literal_39 () String)
(declare-fun x_40 () String)
(declare-fun literal_41 () String)
(declare-fun x_42 () String)
(declare-fun literal_43 () String)
(declare-fun x_44 () String)
(declare-fun literal_45 () String)
(declare-fun x_46 () String)
(declare-fun literal_47 () String)
(declare-fun x_48 () String)
(declare-fun literal_49 () String)
(declare-fun x_50 () String)
(assert (= literal_27 "\x3f\x3d\x26\x3d\x26\x3d\x26\x3d\x26\x3d"))
(assert (= x_28 (str.++ literal_27 sigmaStar_25)))
(assert (= literal_29 "\x26"))
(assert (= x_30 (str.++ x_28 literal_29)))
(assert (= literal_33 "\x3d"))
(assert (= x_35 (str.++ x_30 literal_33)))
(assert (= literal_39 "\x3c\x74\x61\x62\x6c\x65\x20\x77\x69\x64\x74\x68\x3d\x22\x31\x30\x30\x25\x22\x20\x68\x65\x69\x67\x68\x74\x3d\x22\x36\x30\x30\x22\x20\x63\x6c\x61\x73\x73\x3d\x22\x73\x65\x63\x74\x69\x6f\x6e\x22\x3e\x3c\x74\x72\x3e\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x22\x61\x63\x74\x69\x76\x69\x74\x79\x20\x66\x6f\x72\x75\x6d\x22\x3e\x3c\x69\x66\x72\x61\x6d\x65\x20\x6e\x61\x6d\x65\x3d\x22\x69\x66\x72\x61\x6d\x65\x22\x20\x69\x64\x3d\x22\x69\x66\x72\x61\x6d\x65\x22\x20\x73\x72\x63\x3d\x22"))
(assert (= x_40 (str.++ literal_39 x_35)))
(assert (= literal_41 "\x22\x20\x20\x77\x69\x64\x74\x68\x3d\x22\x31\x30\x30\x25\x22\x20\x68\x65\x69\x67\x68\x74\x3d\x22\x31\x30\x30\x25\x22\x20\x66\x72\x61\x6d\x65\x62\x6f\x72\x64\x65\x72\x3d\x22\x31\x22\x3e"))
(assert (= x_42 (str.++ x_40 literal_41)))
(assert (= literal_43 "\x3c\x2f\x69\x66\x72\x61\x6d\x65\x3e"))
(assert (= x_44 (str.++ x_42 literal_43)))
(assert (= literal_45 "\x3c\x2f\x74\x64\x3e"))
(assert (= x_46 (str.++ x_44 literal_45)))
(assert (= literal_47 "\x3c\x2f\x74\x72\x3e"))
(assert (= x_48 (str.++ x_46 literal_47)))
(assert (= literal_49 "\x3c\x2f\x74\x61\x62\x6c\x65\x3e"))
(assert (= x_50 (str.++ x_48 literal_49)))
(assert (str.in.re x_50 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_50 "\x61" 0)) (str.len x_50)))
(check-sat)
(get-model)
