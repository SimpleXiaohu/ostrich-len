(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_7 () String)
(declare-fun x_9 () String)
(declare-fun epsilon () String)
(declare-fun literal_6 () String)
(declare-fun literal_5 () String)
(declare-fun x_13 () String)
(declare-fun literal_14 () String)
(declare-fun x_16 () String)
(declare-fun literal_19 () String)
(declare-fun x_22 () String)
(declare-fun literal_24 () String)
(declare-fun x_26 () String)
(declare-fun literal_27 () String)
(declare-fun x_28 () String)
(assert (= literal_7 "\x69\x64\x3d\x26\x61\x6d\x70\x3b\x73\x69\x64\x3d\x26\x61\x6d\x70\x3b\x67\x72\x6f\x75\x70\x3d"))
(assert (= epsilon ""))
(assert (= literal_6 "\x30"))
(assert (= literal_5 "\x30"))
(assert (or (= x_9 epsilon) (= x_9 literal_6) (= x_9 literal_5)))
(assert (= x_13 (str.++ literal_7 x_9)))
(assert (= literal_14 "\x26\x61\x6d\x70\x3b\x74\x79\x70\x65\x3d\x73\x74\x75\x64\x65\x6e\x74\x2e\x70\x6e\x67"))
(assert (= x_16 (str.++ x_13 literal_14)))
(assert (= literal_19 "\x20\x73\x72\x63\x3d\x5c\x22\x2f\x6d\x6f\x64\x2f\x73\x75\x72\x76\x65\x79\x2f\x67\x72\x61\x70\x68\x2e\x70\x68\x70\x3f"))
(assert (= x_22 (str.++ literal_19 x_16)))
(assert (= literal_24 "\x5c\x22\x20\x61\x6c\x74\x3d\x5c\x22\x5c\x22\x20\x2f\x3e"))
(assert (= x_26 (str.++ x_22 literal_24)))
(assert (= literal_27 "\x3c\x69\x6d\x67\x20\x68\x65\x69\x67\x68\x74\x3d\x5c\x22\x35\x30\x30\x5c\x22\x20\x77\x69\x64\x74\x68\x3d\x5c\x22\x39\x30\x30\x5c\x22\x20\x62\x6f\x72\x64\x65\x72\x3d\x5c\x22\x31\x5c\x22"))
(assert (= x_28 (str.++ literal_27 x_26)))
(assert (str.in.re x_28 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_28 "\x61" 0)) (str.len x_28)))
(check-sat)
(get-model)
