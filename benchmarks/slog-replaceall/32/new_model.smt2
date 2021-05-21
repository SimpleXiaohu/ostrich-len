(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun sigmaStar_2 () String)
(declare-fun sigmaStar_3 () String)
(declare-fun sigmaStar_4 () String)
(declare-fun sigmaStar_5 () String)
(declare-fun sigmaStar_6 () String)
(declare-fun sigmaStar_9 () String)
(declare-fun sigmaStar_16 () String)
(declare-fun sigmaStar_22 () String)
(declare-fun x_10 () String)
(declare-fun sigmaStar_33 () String)
(declare-fun x_17 () String)
(declare-fun sigmaStar_36 () String)
(declare-fun literal_29 () String)
(declare-fun x_23 () String)
(declare-fun x_41 () String)
(declare-fun literal_40 () String)
(declare-fun x_34 () String)
(declare-fun x_46 () String)
(declare-fun literal_42 () String)
(declare-fun x_48 () String)
(declare-fun sigmaStar_49 () String)
(declare-fun x_37 () String)
(declare-fun literal_52 () String)
(declare-fun x_55 () String)
(declare-fun literal_47 () String)
(declare-fun x_56 () String)
(declare-fun literal_53 () String)
(declare-fun x_50 () String)
(declare-fun x_59 () String)
(declare-fun literal_54 () String)
(declare-fun x_62 () String)
(declare-fun literal_58 () String)
(declare-fun x_64 () String)
(declare-fun literal_65 () String)
(declare-fun x_70 () String)
(declare-fun literal_67 () String)
(declare-fun x_73 () String)
(declare-fun literal_68 () String)
(declare-fun x_76 () String)
(declare-fun literal_69 () String)
(declare-fun x_78 () String)
(declare-fun literal_75 () String)
(declare-fun x_84 () String)
(declare-fun literal_77 () String)
(declare-fun x_81 () String)
(declare-fun x_85 () String)
(declare-fun literal_83 () String)
(declare-fun x_87 () String)
(declare-fun x_88 () String)
(declare-fun x_91 () String)
(declare-fun literal_89 () String)
(declare-fun x_93 () String)
(declare-fun x_92 () String)
(declare-fun literal_90 () String)
(declare-fun x_94 () String)
(declare-fun x_95 () String)
(declare-fun literal_96 () String)
(declare-fun x_97 () String)
(declare-fun x_98 () String)
(assert (= x_10 (str.replaceall sigmaStar_3 "\x2f\x3c\x53\x43\x52\x49\x50\x54\x28\x2e\x2a\x3f\x29\x2f" "\x42\x4c\x4f\x43\x4b\x45\x44")))
(assert (= x_17 (str.replaceall sigmaStar_2 "\x2f\x3c\x53\x43\x52\x49\x50\x54\x28\x2e\x2a\x3f\x29\x2f" "\x42\x4c\x4f\x43\x4b\x45\x44")))
(assert (= x_23 (str.replaceall x_10 "\x2f\x3c\x73\x63\x72\x69\x70\x74\x28\x2e\x2a\x3f\x29\x2f" "\x42\x4c\x4f\x43\x4b\x45\x44")))
(assert (= x_34 (str.replaceall x_17 "\x2f\x3c\x73\x63\x72\x69\x70\x74\x28\x2e\x2a\x3f\x29\x2f" "\x42\x4c\x4f\x43\x4b\x45\x44")))
(assert (= x_37 (str.replaceall sigmaStar_5 "\x2f\x3c\x53\x43\x52\x49\x50\x54\x28\x2e\x2a\x3f\x29\x2f" "\x42\x4c\x4f\x43\x4b\x45\x44")))
(assert (= literal_29 "\x26\x6e\x62\x73\x70\x3b\x3c\x41\x20\x53\x54\x59\x4c\x45\x3d\x43\x4f\x4c\x4f\x52\x3a\x62\x6c\x75\x65\x3b\x20\x48\x52\x45\x46\x3d\x5c\x22\x6d\x61\x69\x6c\x74\x6f\x3a"))
(assert (= x_41 (str.++ literal_29 x_23)))
(assert (= literal_40 "\x26\x6e\x62\x73\x70\x3b\x3c\x41\x20\x53\x54\x59\x4c\x45\x3d\x43\x4f\x4c\x4f\x52\x3a\x62\x6c\x75\x65\x3b\x20\x48\x52\x45\x46\x3d\x5c\x22\x68\x74\x74\x70\x3a\x2f\x2f"))
(assert (= x_46 (str.++ literal_40 x_34)))
(assert (= literal_42 "\x5c\x22\x3e\x3c\x49\x4d\x47\x20\x53\x52\x43\x3d"))
(assert (= x_48 (str.++ x_41 literal_42)))
(assert (= x_50 (str.replaceall x_37 "\x2f\x3c\x73\x63\x72\x69\x70\x74\x28\x2e\x2a\x3f\x29\x2f" "\x42\x4c\x4f\x43\x4b\x45\x44")))
(assert (= literal_52 "\x5c\x22\x3e\x3c\x49\x4d\x47\x20\x53\x52\x43\x3d"))
(assert (= x_55 (str.++ x_46 literal_52)))
(assert (= literal_47 "\x68\x74\x74\x70\x3a\x2f\x2f\x6c\x6f\x63\x61\x6c\x68\x6f\x73\x74\x2f\x65\x6d\x61\x69\x6c\x62\x75\x74\x2e\x70\x6e\x67"))
(assert (= x_56 (str.++ x_48 literal_47)))
(assert (= literal_53 "\x26\x6e\x62\x73\x70\x3b\x3c\x41\x20\x53\x54\x59\x4c\x45\x3d\x43\x4f\x4c\x4f\x52\x3a\x62\x6c\x75\x65\x3b\x20\x48\x52\x45\x46\x3d\x70\x62\x6c\x67\x75\x65\x73\x74\x62\x6f\x6f\x6b\x2e\x70\x68\x70\x3f\x61\x63\x74\x69\x6f\x6e\x3d\x64\x65\x6c\x65\x74\x65\x26\x69\x64\x3d"))
(assert (= x_59 (str.++ literal_53 x_50)))
(assert (= literal_54 "\x68\x74\x74\x70\x3a\x2f\x2f\x6c\x6f\x63\x61\x6c\x68\x6f\x73\x74\x2f\x68\x6f\x6d\x65\x62\x75\x74\x2e\x70\x6e\x67"))
(assert (= x_62 (str.++ x_55 literal_54)))
(assert (= literal_58 "\x20\x42\x4f\x52\x44\x45\x52\x3d\x30\x20\x41\x4c\x54\x3d\x5c\x22"))
(assert (= x_64 (str.++ x_56 literal_58)))
(assert (= literal_65 "\x3e\x3c\x49\x4d\x47\x20\x53\x52\x43\x3d"))
(assert (= x_70 (str.++ x_59 literal_65)))
(assert (= literal_67 "\x20\x42\x4f\x52\x44\x45\x52\x3d\x30\x20\x41\x4c\x54\x3d\x5c\x22"))
(assert (= x_73 (str.++ x_62 literal_67)))
(assert (= literal_68 "\x65\x6d\x61\x69\x6c\x22\x3e\x3c\x2f\x41\x3e\x26\x6e\x62\x73\x70\x3b"))
(assert (= x_76 (str.++ x_64 literal_68)))
(assert (= literal_69 "\x68\x74\x74\x70\x3a\x2f\x2f\x6c\x6f\x63\x61\x6c\x68\x6f\x73\x74\x2f\x64\x65\x6c\x62\x75\x74\x2e\x70\x6e\x67"))
(assert (= x_78 (str.++ x_70 literal_69)))
(assert (= literal_75 "\x77\x65\x62\x73\x69\x74\x65\x20\x55\x52\x4c\x22\x3e\x3c\x2f\x41\x3e\x26\x6e\x62\x73\x70\x3b"))
(assert (= x_84 (str.++ x_73 literal_75)))
(assert (= literal_77 "\x3c\x2f\x54\x44\x3e\x3c\x54\x44\x20\x53\x54\x59\x4c\x45\x3d\x54\x45\x58\x54\x2d\x41\x4c\x49\x47\x4e\x3a\x72\x69\x67\x68\x74\x3b\x3e\x3c\x46\x4f\x4e\x54\x20\x53\x49\x5a\x45\x3d\x31\x3e"))
(assert (or (= x_81 x_76) (= x_81 sigmaStar_1)))
(assert (= x_85 (str.++ literal_77 x_81)))
(assert (= literal_83 "\x20\x42\x4f\x52\x44\x45\x52\x3d\x30\x20\x41\x4c\x54\x3d\x5c\x22"))
(assert (= x_87 (str.++ x_78 literal_83)))
(assert (or (= x_88 x_84) (= x_88 sigmaStar_4)))
(assert (= x_91 (str.++ x_85 x_88)))
(assert (= literal_89 "\x64\x65\x6c\x65\x74\x65\x22\x3e\x3c\x2f\x41\x3e\x26\x6e\x62\x73\x70\x3b"))
(assert (= x_93 (str.++ x_87 literal_89)))
(assert (= literal_90 "\x26\x6e\x62\x73\x70\x3b\x3c\x49\x4d\x47\x20\x53\x52\x43\x3d\x68\x74\x74\x70\x3a\x2f\x2f\x6c\x6f\x63\x61\x6c\x68\x6f\x73\x74\x2f\x69\x70\x62\x75\x74\x2e\x70\x6e\x67\x20\x42\x4f\x52\x44\x45\x52\x3d\x30\x20\x41\x4c\x54\x3d\x5c\x22\x22\x3e\x26\x6e\x62\x73\x70\x3b"))
(assert (or (= x_92 literal_90) (= x_92 sigmaStar_0)))
(assert (= x_94 (str.++ x_91 x_92)))
(assert (= x_95 (str.++ x_94 x_93)))
(assert (= literal_96 "\x3c\x2f\x46\x4f\x4e\x54\x3e\x3c\x2f\x54\x44\x3e\x3c\x2f\x54\x52\x3e"))
(assert (= x_97 (str.++ x_95 literal_96)))
(assert (= x_98 (str.++ sigmaStar_6 x_97)))
(assert (str.in.re x_98 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (> (* 2 (str.indexof x_23 "\x61" 0)) (str.len x_23)))
(check-sat)
(get-model)
