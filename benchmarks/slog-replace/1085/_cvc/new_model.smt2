(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun literal_1 () String)
(assert (= literal_1 "\x20\x20\x3c\x74\x64\x3e"))
(assert (str.in.re literal_1 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof literal_1  "\x61" 0)) (str.len literal_1 )))
(check-sat)
(get-model)
