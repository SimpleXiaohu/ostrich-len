(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_5 () String)
(declare-fun x_4 () String)
(declare-fun epsilon () String)
(declare-fun literal_2 () String)
(declare-fun x_7 () String)
(declare-fun literal_10 () String)
(declare-fun x_11 () String)
(declare-fun x_9 () String)
(declare-fun literal_6 () String)
(declare-fun x_12 () String)
(declare-fun literal_13 () String)
(declare-fun x_14 () String)
(assert (= literal_5 "\x3c\x70\x20\x61\x6c\x69\x67\x6e\x3d\x22\x63\x65\x6e\x74\x65\x72\x22\x3e"))
(assert (= epsilon ""))
(assert (= literal_2 "\x30"))
(assert (or (= x_4 epsilon) (= x_4 literal_2)))
(assert (= x_7 (str.++ literal_5 x_4)))
(assert (= literal_10 "\x20\x2d\x2d\x3e\x20"))
(assert (= x_11 (str.++ x_7 literal_10)))
(assert (= literal_6 "\x30"))
(assert (or (= x_9 epsilon) (= x_9 literal_6)))
(assert (= x_12 (str.++ x_11 x_9)))
(assert (= literal_13 "\x3c\x2f\x70\x3e"))
(assert (= x_14 (str.++ x_12 literal_13)))
(assert (str.in.re x_14 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_14 "\x61" 0)) (str.len x_14)))
(check-sat)
(get-model)
