(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_9 () String)
(declare-fun literal_11 () String)
(declare-fun x_10 () String)
(declare-fun epsilon () String)
(declare-fun x_13 () String)
(declare-fun literal_12 () String)
(declare-fun x_14 () String)
(declare-fun literal_15 () String)
(declare-fun x_16 () String)
(declare-fun x_17 () String)
(declare-fun x_18 () String)
(declare-fun literal_19 () String)
(declare-fun x_20 () String)
(declare-fun x_21 () String)
(declare-fun literal_22 () String)
(declare-fun x_23 () String)
(assert (= literal_11 "\x3c\x62\x3e"))
(assert (= epsilon ""))
(assert (or (= x_10 epsilon)))
(assert (= x_13 (str.++ literal_11 x_10)))
(assert (= literal_12 "\x3f\x61\x69\x64\x3d"))
(assert (= x_14 (str.++ literal_12 sigmaStar_9)))
(assert (= literal_15 "\x3c\x2f\x62\x3e\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x5c\x6e\x3c\x61\x20\x68\x72\x65\x66\x3d\x27\x70\x68\x6f\x74\x6f\x73\x2e\x70\x68\x70"))
(assert (= x_16 (str.++ x_13 literal_15)))
(assert (or (= x_17 sigmaStar_0) (= x_17 x_14)))
(assert (= x_18 (str.++ x_16 x_17)))
(assert (= literal_19 "\x26\x61\x6d\x70\x3b\x61\x6c\x62\x75\x6d\x5f\x69\x64\x3d"))
(assert (= x_20 (str.++ x_18 literal_19)))
(assert (= x_21 (str.++ x_20 x_10)))
(assert (= literal_22 "\x27\x3e"))
(assert (= x_23 (str.++ x_21 literal_22)))
(assert (str.in.re x_23 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_23 "\x61" 0)) (str.len x_23)))
(check-sat)
(get-model)
