(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_5 () String)
(declare-fun x_4 () String)
(declare-fun literal_2 () String)
(declare-fun literal_3 () String)
(declare-fun x_6 () String)
(declare-fun literal_35 () String)
(declare-fun x_48 () String)
(declare-fun x_49 () String)
(declare-fun literal_50 () String)
(declare-fun x_51 () String)
(assert (= literal_5 "\x2f"))
(assert (= literal_2 "\x77\x64\x69\x72"))
(assert (= literal_3 "\x2f"))
(assert (or (= x_4 literal_2) (= x_4 literal_3)))
(assert (= x_6 (str.++ literal_5 x_4)))
(assert (= literal_35 "\x20\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x5c\x22\x68\x69\x64\x64\x65\x6e\x5c\x22\x20\x6e\x61\x6d\x65\x3d\x5c\x22\x77\x64\x69\x72\x5c\x22\x20\x76\x61\x6c\x75\x65\x3d\x5c\x22"))
(assert (or (= x_48 literal_2) (= x_48 literal_3) (= x_48 x_6)))
(assert (= x_49 (str.++ literal_35 x_48)))
(assert (= literal_50 "\x5c\x22\x20\x2f\x3e"))
(assert (= x_51 (str.++ x_49 literal_50)))
(assert (str.in.re x_51 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_51 "\x61" 0)) (str.len x_51)))
(check-sat)
(get-model)
