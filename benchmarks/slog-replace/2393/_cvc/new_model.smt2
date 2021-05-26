(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_3 () String)
(declare-fun x_2 () String)
(declare-fun epsilon () String)
(declare-fun literal_1 () String)
(declare-fun x_4 () String)
(declare-fun literal_5 () String)
(declare-fun x_6 () String)
(declare-fun literal_7 () String)
(declare-fun x_8 () String)
(declare-fun literal_9 () String)
(declare-fun x_10 () String)
(assert (= literal_3 "\x3c\x61\x20\x74\x69\x74\x6c\x65\x3d\x22"))
(assert (= epsilon ""))
(assert (= literal_1 "\x72\x65\x71\x75\x69\x72\x65\x73\x6b\x65\x79"))
(assert (or (= x_2 epsilon) (= x_2 literal_1)))
(assert (= x_4 (str.++ literal_3 x_2)))
(assert (= literal_5 "\x22\x20\x68\x72\x65\x66\x3d\x22"))
(assert (= x_6 (str.++ x_4 literal_5)))
(assert (= literal_7 "\x2f\x63\x6f\x75\x72\x73\x65\x2f\x76\x69\x65\x77\x2e\x70\x68\x70\x3f\x69\x64\x3d"))
(assert (= x_8 (str.++ x_6 literal_7)))
(assert (= literal_9 "\x22\x3e"))
(assert (= x_10 (str.++ x_8 literal_9)))
(assert (str.in.re x_10 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_10 "\x61" 0)) (str.len x_10)))
(check-sat)
(get-model)