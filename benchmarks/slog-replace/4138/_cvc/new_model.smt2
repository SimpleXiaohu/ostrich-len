(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_4 () String)
(declare-fun literal_5 () String)
(declare-fun x_6 () String)
(declare-fun sigmaStar_7 () String)
(declare-fun literal_8 () String)
(declare-fun x_9 () String)
(declare-fun x_10 () String)
(declare-fun x_11 () String)
(declare-fun literal_12 () String)
(declare-fun x_13 () String)
(declare-fun sigmaStar_14 () String)
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
(assert (= literal_5 "\x3c\x61\x20\x68\x72\x65\x66\x3d\x27\x76\x69\x65\x77\x74\x68\x72\x65\x61\x64\x2e\x70\x68\x70\x3f\x66\x6f\x72\x75\x6d\x5f\x69\x64\x3d"))
(assert (= x_6 (str.++ literal_5 sigmaStar_2)))
(assert (= literal_8 "\x26\x61\x6d\x70\x3b\x74\x68\x72\x65\x61\x64\x5f\x69\x64\x3d"))
(assert (= x_9 (str.++ x_6 literal_8)))
(assert (or (= x_10 sigmaStar_7) (= x_10 sigmaStar_4)))
(assert (= x_11 (str.++ x_9 x_10)))
(assert (= literal_12 "\x26\x61\x6d\x70\x3b\x70\x69\x64\x3d"))
(assert (= x_13 (str.++ x_11 literal_12)))
(assert (or (= x_15 sigmaStar_14) (= x_15 sigmaStar_1)))
(assert (= x_16 (str.++ x_13 x_15)))
(assert (= literal_17 "\x23\x70\x6f\x73\x74\x5f"))
(assert (= x_18 (str.++ x_16 literal_17)))
(assert (or (= x_19 sigmaStar_14) (= x_19 sigmaStar_3)))
(assert (= x_20 (str.++ x_18 x_19)))
(assert (= literal_21 "\x27\x3e"))
(assert (= x_22 (str.++ x_20 literal_21)))
(assert (= x_23 (str.++ x_22 sigmaStar_0)))
(assert (= literal_24 "\x3c\x2f\x61\x3e\x20\x7c\x5c\x6e"))
(assert (= x_25 (str.++ x_23 literal_24)))
(assert (str.in.re x_25 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_25 "\x61" 0)) (str.len x_25)))
(check-sat)
(get-model)
