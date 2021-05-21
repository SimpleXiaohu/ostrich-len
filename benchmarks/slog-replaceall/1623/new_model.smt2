(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_5 () String)
(declare-fun sigmaStar_8 () String)
(declare-fun x_4 () String)
(declare-fun x_9 () String)
(declare-fun epsilon () String)
(declare-fun sigmaStar_10 () String)
(declare-fun sigmaStar_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_14 () String)
(declare-fun literal_15 () String)
(declare-fun x_16 () String)
(assert (= epsilon ""))
(assert (or (= x_4 epsilon) (= x_4 sigmaStar_2)))
(assert (= x_9 (str.replaceall x_4 "\x2f\x2e\x28\x5c\x64\x2b\x29\x2e\x2f" "\x5f\x24\x31\x2e")))
(assert (= literal_13 "\x20\x20\x20\x20"))
(assert (= x_14 (str.++ literal_13 sigmaStar_12)))
(assert (= literal_15 "\x2e\x73\x63\x6f\x72\x65\x2e\x6d\x69\x6e\x20\x3d\x20\x27\x27\x3b\x5c\x6e"))
(assert (= x_16 (str.++ x_14 literal_15)))
(assert (str.in.re x_16 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (> (* 2 (str.indexof x_9 "\x61" 0)) (str.len x_9)))
(check-sat)
(get-model)
