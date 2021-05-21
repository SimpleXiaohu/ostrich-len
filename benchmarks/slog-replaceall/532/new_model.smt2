(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_4 () String)
(declare-fun literal_6 () String)
(declare-fun x_5 () String)
(declare-fun x_7 () String)
(declare-fun literal_8 () String)
(declare-fun x_9 () String)
(declare-fun x_10 () String)
(declare-fun literal_11 () String)
(declare-fun x_12 () String)
(assert (= x_5 (str.replaceall sigmaStar_0 "\x2f\x2e\x28\x5c\x64\x2b\x29\x2e\x2f" "\x5f\x24\x31\x2e")))
(assert (= literal_6 "\x20\x20\x20\x20"))
(assert (= x_7 (str.++ literal_6 x_5)))
(assert (= literal_8 "\x20\x3d\x20\x27"))
(assert (= x_9 (str.++ x_7 literal_8)))
(assert (= x_10 (str.++ x_9 sigmaStar_0)))
(assert (= literal_11 "\x27\x3b\x5c\x6e"))
(assert (= x_12 (str.++ x_10 literal_11)))
(assert (str.in.re x_12 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (> (* 2 (str.indexof x_7 "\x61" 0)) (str.len x_7)))
(check-sat)
(get-model)
