(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)
(declare-fun sigmaStar_0 () String)
(declare-fun sigmaStar_1 () String)
(declare-fun literal_2 () String)
(declare-fun x_3 () String)
(declare-fun literal_4 () String)
(declare-fun x_5 () String)
(declare-fun x_6 () String)
(declare-fun literal_7 () String)
(declare-fun x_8 () String)
(assert (= literal_2 "\x0d\x0a\x20\x3c\x74\x72\x3e\x0d\x0a\x20\x20\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27\x62\x27\x20\x77\x69\x64\x74\x68\x3d\x31\x33\x30\x20\x76\x61\x6c\x69\x67\x6e\x3d\x27\x74\x6f\x70\x27\x3e\x0d\x0a\x20\x20\x20\x3c\x62\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x66\x6f\x72\x6d\x20\x6e\x61\x6d\x65\x3d\x27\x61\x64\x6d\x69\x6e\x27\x20\x61\x63\x74\x69\x6f\x6e\x3d\x27\x2e\x2f\x69\x6e\x64\x65\x78\x2e\x70\x68\x70\x27\x20\x6d\x65\x74\x68\x6f\x64\x3d\x27\x50\x4f\x53\x54\x27\x3e\x0d\x0a\x0d\x0a\x20\x20\x20\x3c\x61\x20\x63\x6c\x61\x73\x73\x3d\x27\x6d\x65\x6e\x75\x27\x20\x68\x72\x65\x66\x3d\x27\x6a\x61\x76\x61\x73\x63\x72\x69\x70\x74\x3a\x20\x73\x63\x68\x6f\x6f\x6c\x49\x6e\x66\x6f\x28\x29\x3b\x27\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x76\x65\x72\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x4d\x61\x6e\x61\x67\x65\x20\x53\x63\x68\x6f\x6f\x6c\x20\x49\x6e\x66\x6f\x72\x6d\x61\x74\x69\x6f\x6e\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x75\x74\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x3e\x53\x63\x68\x6f\x6f\x6c\x3c\x2f\x61\x3e\x0d\x0a\x20\x20\x20\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x61\x20\x63\x6c\x61\x73\x73\x3d\x27\x6d\x65\x6e\x75\x27\x20\x68\x72\x65\x66\x3d\x27\x6a\x61\x76\x61\x73\x63\x72\x69\x70\x74\x3a\x20\x74\x65\x72\x6d\x73\x28\x29\x3b\x27\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x76\x65\x72\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x4d\x61\x6e\x61\x67\x65\x20\x54\x65\x72\x6d\x73\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x75\x74\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x3e\x54\x65\x72\x6d\x73\x3c\x2f\x61\x3e\x0d\x0a\x20\x20\x20\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x61\x20\x63\x6c\x61\x73\x73\x3d\x27\x6d\x65\x6e\x75\x27\x20\x68\x72\x65\x66\x3d\x27\x6a\x61\x76\x61\x73\x63\x72\x69\x70\x74\x3a\x20\x73\x65\x6d\x65\x73\x74\x65\x72\x73\x28\x29\x3b\x27\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x76\x65\x72\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x4d\x61\x6e\x61\x67\x65\x20\x53\x65\x6d\x65\x73\x74\x65\x72\x73\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x75\x74\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x3e\x53\x65\x6d\x65\x73\x74\x65\x72\x73\x3c\x2f\x61\x3e\x0d\x0a\x20\x20\x20\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x61\x20\x63\x6c\x61\x73\x73\x3d\x27\x6d\x65\x6e\x75\x27\x20\x68\x72\x65\x66\x3d\x27\x6a\x61\x76\x61\x73\x63\x72\x69\x70\x74\x3a\x20\x63\x6c\x61\x73\x73\x65\x73\x28\x29\x3b\x27\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x76\x65\x72\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x4d\x61\x6e\x61\x67\x65\x20\x43\x6c\x61\x73\x73\x65\x73\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x75\x74\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x3e\x43\x6c\x61\x73\x73\x65\x73\x3c\x2f\x61\x3e\x0d\x0a\x20\x20\x20\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x61\x20\x63\x6c\x61\x73\x73\x3d\x27\x6d\x65\x6e\x75\x27\x20\x68\x72\x65\x66\x3d\x27\x6a\x61\x76\x61\x73\x63\x72\x69\x70\x74\x3a\x20\x75\x73\x65\x72\x73\x28\x29\x3b\x27\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x76\x65\x72\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x4d\x61\x6e\x61\x67\x65\x20\x55\x73\x65\x72\x73\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x75\x74\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x3e\x55\x73\x65\x72\x73\x3c\x2f\x61\x3e\x0d\x0a\x20\x20\x20\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x61\x20\x63\x6c\x61\x73\x73\x3d\x27\x6d\x65\x6e\x75\x27\x20\x68\x72\x65\x66\x3d\x27\x6a\x61\x76\x61\x73\x63\x72\x69\x70\x74\x3a\x20\x74\x65\x61\x63\x68\x65\x72\x73\x28\x29\x3b\x27\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x76\x65\x72\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x4d\x61\x6e\x61\x67\x65\x20\x54\x65\x61\x63\x68\x65\x72\x73\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x75\x74\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x3e\x54\x65\x61\x63\x68\x65\x72\x73\x3c\x2f\x61\x3e\x0d\x0a\x20\x20\x20\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x61\x20\x63\x6c\x61\x73\x73\x3d\x27\x6d\x65\x6e\x75\x27\x20\x68\x72\x65\x66\x3d\x27\x6a\x61\x76\x61\x73\x63\x72\x69\x70\x74\x3a\x20\x73\x74\x75\x64\x65\x6e\x74\x73\x28\x29\x3b\x27\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x76\x65\x72\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x4d\x61\x6e\x61\x67\x65\x20\x53\x74\x75\x64\x65\x6e\x74\x73\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x75\x74\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x3e\x53\x74\x75\x64\x65\x6e\x74\x73\x3c\x2f\x61\x3e\x0d\x0a\x20\x20\x20\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x61\x20\x63\x6c\x61\x73\x73\x3d\x27\x6d\x65\x6e\x75\x27\x20\x68\x72\x65\x66\x3d\x27\x6a\x61\x76\x61\x73\x63\x72\x69\x70\x74\x3a\x20\x72\x65\x67\x69\x73\x74\x65\x72\x28\x29\x3b\x27\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x76\x65\x72\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x52\x65\x67\x69\x73\x74\x65\x72\x20\x53\x74\x75\x64\x65\x6e\x74\x73\x20\x66\x6f\x72\x20\x43\x6c\x61\x73\x73\x65\x73\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x75\x74\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x3e\x52\x65\x67\x69\x73\x74\x72\x61\x74\x69\x6f\x6e\x3c\x2f\x61\x3e\x0d\x0a\x20\x20\x20\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x61\x20\x63\x6c\x61\x73\x73\x3d\x27\x6d\x65\x6e\x75\x27\x20\x68\x72\x65\x66\x3d\x27\x6a\x61\x76\x61\x73\x63\x72\x69\x70\x74\x3a\x20\x61\x74\x74\x65\x6e\x64\x61\x6e\x63\x65\x28\x29\x3b\x27\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x76\x65\x72\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x4b\x65\x65\x70\x20\x41\x74\x74\x65\x6e\x64\x61\x6e\x63\x65\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x75\x74\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x3e\x41\x74\x74\x65\x6e\x64\x61\x6e\x63\x65\x3c\x2f\x61\x3e\x0d\x0a\x20\x20\x20\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x61\x20\x63\x6c\x61\x73\x73\x3d\x27\x6d\x65\x6e\x75\x27\x20\x68\x72\x65\x66\x3d\x27\x6a\x61\x76\x61\x73\x63\x72\x69\x70\x74\x3a\x20\x70\x61\x72\x65\x6e\x74\x73\x28\x29\x3b\x27\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x76\x65\x72\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x4d\x61\x6e\x61\x67\x65\x20\x50\x61\x72\x65\x6e\x74\x73\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x75\x74\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x3e\x50\x61\x72\x65\x6e\x74\x73\x3c\x2f\x61\x3e\x0d\x0a\x20\x20\x20\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x61\x20\x63\x6c\x61\x73\x73\x3d\x27\x6d\x65\x6e\x75\x27\x20\x68\x72\x65\x66\x3d\x27\x6a\x61\x76\x61\x73\x63\x72\x69\x70\x74\x3a\x20\x61\x6e\x6e\x6f\x75\x6e\x63\x65\x6d\x65\x6e\x74\x73\x28\x29\x3b\x27\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x76\x65\x72\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x4d\x61\x6e\x61\x67\x65\x20\x41\x6e\x6e\x6f\x75\x6e\x63\x65\x6d\x65\x6e\x74\x73\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x75\x74\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x27\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x3e\x41\x6e\x6e\x6f\x75\x6e\x63\x65\x6d\x65\x6e\x74\x73\x3c\x2f\x61\x3e\x0d\x0a\x20\x20\x20\x3c\x62\x72\x3e\x3c\x62\x72\x3e\x0d\x0a\x20\x20\x20\x3c\x61\x20\x63\x6c\x61\x73\x73\x3d\x27\x6d\x65\x6e\x75\x27\x20\x68\x72\x65\x66\x3d\x27\x6a\x61\x76\x61\x73\x63\x72\x69\x70\x74\x3a\x20\x6c\x6f\x67\x6f\x75\x74\x41\x64\x6d\x69\x6e\x28\x29\x3b\x27\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x76\x65\x72\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x4c\x6f\x67\x20\x4f\x75\x74\x27\x3b\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x20\x6f\x6e\x4d\x6f\x75\x73\x65\x6f\x75\x74\x3d\x5c\x22\x77\x69\x6e\x64\x6f\x77\x2e\x73\x74\x61\x74\x75\x73\x3d\x27\x27\x3b\x72\x65\x74\x75\x72\x6e\x20\x74\x72\x75\x65\x3b\x5c\x22\x3e\x4c\x6f\x67\x20\x4f\x75\x74\x3c\x2f\x61\x3e\x0d\x0a\x0d\x0a\x20\x20\x20\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x27\x68\x69\x64\x64\x65\x6e\x27\x20\x6e\x61\x6d\x65\x3d\x27\x70\x61\x67\x65\x32\x27\x20\x76\x61\x6c\x75\x65\x3d\x27"))
(assert (= x_3 (str.++ literal_2 sigmaStar_0)))
(assert (= literal_4 "\x3e\x0d\x0a\x20\x20\x20\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x68\x69\x64\x64\x65\x6e\x27\x20\x6e\x61\x6d\x65\x3d\x27\x6c\x6f\x67\x6f\x75\x74\x27\x3e\x0d\x0a\x20\x20\x20\x3c\x69\x6e\x70\x75\x74\x20\x74\x79\x70\x65\x3d\x27\x68\x69\x64\x64\x65\x6e\x27\x20\x6e\x61\x6d\x65\x3d\x27\x70\x61\x67\x65\x27\x20\x76\x61\x6c\x75\x65\x3d\x27"))
(assert (= x_5 (str.++ x_3 literal_4)))
(assert (= x_6 (str.++ x_5 sigmaStar_1)))
(assert (= literal_7 "\x3e\x0d\x0a\x20\x3c\x2f\x66\x6f\x72\x6d\x3e\x0d\x0a\x20\x20\x3c\x2f\x74\x64\x3e\x0d\x0a\x20\x20\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x62\x27\x20\x77\x69\x64\x74\x68\x3d\x31\x30\x20\x62\x61\x63\x6b\x67\x72\x6f\x75\x6e\x64\x3d\x27\x2e\x2f\x69\x6d\x61\x67\x65\x73\x2f\x6c\x65\x66\x74\x2e\x67\x69\x66\x27\x3e\x3c\x64\x69\x76\x20\x73\x74\x79\x6c\x65\x3d\x27\x6c\x65\x74\x74\x65\x72\x2d\x73\x70\x61\x63\x69\x6e\x67\x3a\x20\x31\x70\x74\x3b\x27\x3e\x26\x6e\x62\x73\x70\x3b\x3c\x2f\x64\x69\x76\x3e\x3c\x2f\x74\x64\x3e\x0d\x0a\x20\x20\x3c\x74\x64\x20\x63\x6c\x61\x73\x73\x3d\x27\x77\x27\x20\x76\x61\x6c\x69\x67\x6e\x3d\x27\x74\x6f\x70\x27\x3e\x0d\x0a\x20\x20\x20\x3c\x74\x61\x62\x6c\x65\x20\x62\x6f\x72\x64\x65\x72\x3d\x30\x20\x63\x65\x6c\x6c\x73\x70\x61\x63\x69\x6e\x67\x3d\x30\x20\x63\x65\x6c\x6c\x70\x61\x64\x64\x69\x6e\x67\x3d\x31\x30\x20\x77\x69\x64\x74\x68\x3d\x27\x31\x30\x30\x25\x27\x20\x68\x65\x69\x67\x68\x74\x3d\x27\x31\x30\x30\x25\x27\x3e\x0d\x0a\x09\x3c\x74\x72\x3e\x0d\x0a\x09\x20\x3c\x74\x64\x20\x76\x61\x6c\x69\x67\x6e\x3d\x27\x74\x6f\x70\x27\x3e"))
(assert (= x_8 (str.++ x_6 literal_7)))
(assert (str.in.re x_8 (re.++ (re.* re.allchar) (re.++ (str.to.re "\x5c\x3c\x53\x43\x52\x49\x50\x54") (re.* re.allchar)))))
(assert (< (* 2 (str.indexof x_8 "\x61" 0)) (str.len x_8)))
(check-sat)
(get-model)
