(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun x_3 () String)
(declare-fun literal_0 () String)
(declare-fun literal_1 () String)
(declare-fun literal_2 () String)
(declare-fun x_4 () String)
(assert (= literal_0 "\x2e\x2e\x2f\x69\x6e\x63\x6c\x75\x64\x65\x2f"))
(assert (= literal_1 "\x69\x6e\x63\x6c\x75\x64\x65\x2f"))
(assert (or (= x_3 literal_0) (= x_3 literal_1)))
(assert (= literal_2 "\x61\x75\x74\x68\x2e\x70\x68\x70"))
(assert (= x_4 (str.++ x_3 literal_2)))
(assert (str.in.re x_4 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x2f\x65\x76\x69\x6c") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_4 "\x61" 0)) (str.len x_4)))
(check-sat)
(get-model)
