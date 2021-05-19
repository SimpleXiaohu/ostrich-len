(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_6 () String)
(declare-fun x_2 () String)
(declare-fun epsilon () String)
(declare-fun literal_8 () String)
(declare-fun x_7 () String)
(declare-fun x_9 () String)
(declare-fun literal_10 () String)
(declare-fun x_11 () String)
(declare-fun x_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_14 () String)
(assert (= epsilon ""))
(assert (or (= x_2 epsilon) (= x_2 sigmaStar_0)))
(assert (= x_7 (str.replaceall x_2 "\x2f\x2e\x28\x5c\x64\x2b\x29\x2e\x2f" "\x5f\x24\x31\x2e")))
(assert (= literal_8 "\x20\x20\x20\x20"))
(assert (= x_9 (str.++ literal_8 x_7)))
(assert (= literal_10 "\x20\x3d\x20\x27"))
(assert (= x_11 (str.++ x_9 literal_10)))
(assert (= x_12 (str.++ x_11 x_2)))
(assert (= literal_13 "\x27\x3b\x5c\x6e"))
(assert (= x_14 (str.++ x_12 literal_13)))
(assert (str.in.re x_14 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (> (* 2 (str.indexof x_9 "\x61" 0)) (str.len x_9)))
(check-sat)
(get-model)
