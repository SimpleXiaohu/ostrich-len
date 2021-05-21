(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_2 () String)
(declare-fun x_3 () String)
(declare-fun literal_4 () String)
(declare-fun x_6 () String)
(declare-fun literal_5 () String)
(declare-fun x_7 () String)
(assert (= literal_2 "\x44\x52\x4f\x50\x20\x54\x41\x42\x4c\x45\x20\x49\x46\x20\x45\x58\x49\x53\x54\x53\x20\x60"))
(assert (= x_3 (str.++ literal_2 sigmaStar_0)))
(assert (= literal_4 "\x60\x3b"))
(assert (= x_6 (str.++ x_3 literal_4)))
(assert (= literal_5 "\x5c\x6e"))
(assert (= x_7 (str.++ x_6 literal_5)))
(assert (str.in.re x_7 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_7 "\x61" 0)) (str.len x_7)))
(check-sat)
(get-model)
