(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun literal_3 () String)
(declare-fun x_4 () String)
(declare-fun sigmaStar_5 () String)
(declare-fun literal_6 () String)
(declare-fun x_7 () String)
(declare-fun x_8 () String)
(declare-fun x_9 () String)
(declare-fun literal_10 () String)
(declare-fun x_11 () String)
(declare-fun x_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_14 () String)
(assert (= literal_3 "\x3c\x61\x20\x68\x72\x65\x66\x3d\x27\x76\x69\x65\x77\x74\x68\x72\x65\x61\x64\x2e\x70\x68\x70\x3f\x66\x6f\x72\x75\x6d\x5f\x69\x64\x3d"))
(assert (= x_4 (str.++ literal_3 sigmaStar_1)))
(assert (= literal_6 "\x26\x61\x6d\x70\x3b\x74\x68\x72\x65\x61\x64\x5f\x69\x64\x3d"))
(assert (= x_7 (str.++ x_4 literal_6)))
(assert (or (= x_8 sigmaStar_2) (= x_8 sigmaStar_5)))
(assert (= x_9 (str.++ x_7 x_8)))
(assert (= literal_10 "\x27\x3e"))
(assert (= x_11 (str.++ x_9 literal_10)))
(assert (= x_12 (str.++ x_11 sigmaStar_0)))
(assert (= literal_13 "\x3c\x2f\x61\x3e\x20\x7c\x5c\x6e"))
(assert (= x_14 (str.++ x_12 literal_13)))
(assert (str.in.re x_14 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_14 "\x61" 0)) (str.len x_14)))
(check-sat)
(get-model)
