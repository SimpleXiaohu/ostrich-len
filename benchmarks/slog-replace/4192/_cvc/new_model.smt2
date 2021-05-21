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
(declare-fun sigmaStar_8 () String)
(declare-fun sigmaStar_9 () String)
(declare-fun literal_10 () String)
(declare-fun x_12 () String)
(declare-fun literal_11 () String)
(declare-fun x_13 () String)
(declare-fun literal_14 () String)
(declare-fun x_15 () String)
(declare-fun x_16 () String)
(declare-fun literal_17 () String)
(declare-fun x_19 () String)
(declare-fun literal_18 () String)
(declare-fun x_20 () String)
(declare-fun x_21 () String)
(declare-fun x_22 () String)
(declare-fun literal_23 () String)
(declare-fun x_24 () String)
(declare-fun x_25 () String)
(declare-fun x_26 () String)
(declare-fun literal_27 () String)
(declare-fun x_28 () String)
(assert (= literal_10 "\x52\x45\x3a\x20"))
(assert (= x_12 (str.++ literal_10 sigmaStar_1)))
(assert (= literal_11 "\x52\x45\x3a\x20"))
(assert (= x_13 (str.++ literal_11 sigmaStar_4)))
(assert (= literal_14 "\x3c\x2f\x74\x64\x3e\x0d\x0a\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27\x74\x62\x6c\x32\x27\x3e"))
(assert (or (= x_15 x_13) (= x_15 x_12)))
(assert (= x_16 (str.++ literal_14 x_15)))
(assert (= literal_17 "\x3c\x2f\x74\x64\x3e\x0d\x0a\x3c\x2f\x74\x72\x3e\x0d\x0a\x3c\x74\x72\x3e\x0d\x0a\x3c\x74\x64\x20\x76\x61\x6c\x69\x67\x6e\x3d\x27\x74\x6f\x70\x27\x20\x72\x6f\x77\x73\x70\x61\x6e\x3d\x27\x32\x27\x20\x77\x69\x64\x74\x68\x3d\x27\x31\x34\x35\x27\x20\x63\x6c\x61\x73\x73\x3d\x27\x74\x62\x6c\x31\x27\x3e"))
(assert (= x_19 (str.++ x_16 literal_17)))
(assert (= literal_18 "\x3c\x74\x61\x62\x6c\x65\x20\x63\x65\x6c\x6c\x70\x61\x64\x64\x69\x6e\x67\x3d\x27\x30\x27\x20\x63\x65\x6c\x6c\x73\x70\x61\x63\x69\x6e\x67\x3d\x27\x30\x27\x20\x77\x69\x64\x74\x68\x3d\x27\x31\x30\x30\x25\x27\x20\x63\x6c\x61\x73\x73\x3d\x27\x74\x62\x6c\x2d\x62\x6f\x72\x64\x65\x72\x27\x3e\x0d\x0a\x3c\x74\x72\x3e\x0d\x0a\x3c\x74\x64\x3e\x0d\x0a\x3c\x74\x61\x62\x6c\x65\x20\x63\x65\x6c\x6c\x70\x61\x64\x64\x69\x6e\x67\x3d\x27\x30\x27\x20\x63\x65\x6c\x6c\x73\x70\x61\x63\x69\x6e\x67\x3d\x27\x31\x27\x20\x77\x69\x64\x74\x68\x3d\x27\x31\x30\x30\x25\x27\x3e\x0d\x0a\x3c\x74\x72\x3e\x0d\x0a\x3c\x74\x64\x20\x77\x69\x64\x74\x68\x3d\x27\x31\x34\x35\x27\x20\x63\x6c\x61\x73\x73\x3d\x27\x74\x62\x6c\x32\x27\x3e"))
(assert (= x_20 (str.++ literal_18 sigmaStar_3)))
(assert (= x_21 (str.++ x_20 x_19)))
(assert (= x_22 (str.++ x_21 sigmaStar_6)))
(assert (= literal_23 "\x3c\x62\x72\x3e\x0d\x0a\x3c\x73\x70\x61\x6e\x20\x63\x6c\x61\x73\x73\x3d\x27\x61\x6c\x74\x27\x3e"))
(assert (= x_24 (str.++ x_22 literal_23)))
(assert (or (= x_25 sigmaStar_2) (= x_25 sigmaStar_7)))
(assert (= x_26 (str.++ x_24 x_25)))
(assert (= literal_27 "\x3c\x2f\x73\x70\x61\x6e\x3e\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x5c\x6e"))
(assert (= x_28 (str.++ x_26 literal_27)))
(assert (str.in.re x_28 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_28 "\x61" 0)) (str.len x_28)))
(check-sat)
(get-model)
