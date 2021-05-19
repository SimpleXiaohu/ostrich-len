(set-logic QF_S)
(set-option :strings-exp true)
(set-option :produce-models true)

; ----------------
; trim
; -----------------
; http://php.net/manual/en/function.trim.php
; Strip whitespace from the beginning and end of a string
;    " "    (ASCII 32 (0x20)), an ordinary space.
;    "\t"   (ASCII 9  (0x09)), a tab.
;    "\n"   (ASCII 10 (0x0A)), a new line (line feed).
;    "\r"   (ASCII 13 (0x0D)), a carriage return.
;    "\0"   (ASCII 0  (0x00)), the NUL-byte.
;    "\x0B" (ASCII 11 (0x0B)), a vertical tab.


(define-funs-rec ((trim  ((x String) (y String)) Bool)
                  (tr1  ((x String) (y String)) Bool)
                  (tr2  ((x String) (y String)) Bool)
                  (tr3  ((x String) (y String)) Bool)) (

                  ; definition of trim
                  (or (and (= x "") (= y ""))
                      (and (not (= x "")) 
                      (and (= (str.head x) (char.from-int 32)) ; ' '
                           (trim (str.tail x) y)))
                      (and (not (= x "")) 
                      (and (= (str.head x) (char.from-int 09)) ; '\t'
                           (trim (str.tail x) y)))
                      (and (not (= x ""))
                      (and (= (str.head x) (char.from-int 10)) ; '\n'
                           (trim (str.tail x) y)))
                      (and (not (= x ""))
                      (and (= (str.head x) (char.from-int 13 )) ; '\r'
                           (trim (str.tail x) y)))
                      (and (not (= x ""))
                      (and (= (str.head x) (char.from-int 0  )) ; '\x00'
                           (trim (str.tail x) y)))
                      (and (not (= x ""))
                      (and (= (str.head x) (char.from-int 11 )) ; '\x0B'
                           (trim (str.tail x) y)))
                      (and (not (= x "")) (not (= y ""))
					  (and (= (str.head x) (str.head y))            ; x <=> y
                           (and (not (= (str.head x) (char.from-int 32 )))  ; '\''
                                (not (= (str.head x) (char.from-int 09 )))  ; '"'
                                (not (= (str.head x) (char.from-int 10 )))  ; '\'
                                (not (= (str.head x) (char.from-int 13 )))  ; '\'
                                (not (= (str.head x) (char.from-int 11 )))  ; '\'
                                (not (= (str.head x) (char.from-int 0  )))) ; '\x00'
                           (tr1 (str.tail x) (str.tail y)))))

                  ; definition of tr1
                  (or 
                      (and (= x "") (= y ""))
                      (and (not (= x "")) (not (= y ""))
                      (and (= (str.head x) (char.from-int 32)) ; ' '
                           (= (str.head y) (char.from-int 32)) ; ' '
                           (tr2 (str.tail x) (str.tail y))))
                      (and (not (= x "")) (not (= y ""))
                      (and (= (str.head x) (char.from-int 09)) ; '\t'
                      	   (= (str.head y) (char.from-int 09)) ; '\t'
                           (tr2 (str.tail x) (str.tail y))))
                      (and (not (= x "")) (not (= y ""))
                      (and (= (str.head x) (char.from-int 10)) ; '\n'
                           (= (str.head y) (char.from-int 10)) ; '\n'
                           (tr2 (str.tail x) (str.tail y))))
                      (and (not (= x "")) (not (= y ""))
                      (and (= (str.head x) (char.from-int 13)) ; '\r'
                           (= (str.head y) (char.from-int 13)) ; '\r'
                           (tr2 (str.tail x) (str.tail y))))
                      (and (not (= x "")) (not (= y ""))
                      (and (= (str.head x) (char.from-int 0 )) ; '\x00'
                           (= (str.head y) (char.from-int 0 )) ; '\x00'
                           (tr2 (str.tail x) (str.tail y))))
                      (and (not (= x "")) (not (= y ""))
                      (and (= (str.head x) (char.from-int 11)) ; '\x0B'
                           (= (str.head y) (char.from-int 11)) ; '\x0B'
                           (tr2 (str.tail x) (str.tail y))))
                      (and (not (= x ""))
                      (and (= (str.head x) (char.from-int 32)) ; ' '
                           (tr3 (str.tail x) y)))
                      (and (not (= x ""))
                      (and (= (str.head x) (char.from-int 09)) ; '\t'
                           (tr3 (str.tail x) y)))
                      (and (not (= x ""))
                      (and (= (str.head x) (char.from-int 10)) ; '\n'
                           (tr3 (str.tail x) y)))
                      (and (not (= x ""))
                      (and (= (str.head x) (char.from-int 13 )) ; '\r'
                           (tr3 (str.tail x) y)))
                      (and (not (= x ""))
                      (and (= (str.head x) (char.from-int 0  )) ; '\x00'
                           (tr3 (str.tail x) y)))
                      (and (not (= x ""))
                      (and (= (str.head x) (char.from-int 11 )) ; '\x0B'
                           (tr3 (str.tail x) y)))
                      (and (not (= x "")) (not (= y ""))
					  (and (= (str.head x) (str.head y))            ; x <=> y
                           (and (not (= (str.head x) (char.from-int 32 )))  ; '\''
                                (not (= (str.head x) (char.from-int 09 )))  ; '"'
                                (not (= (str.head x) (char.from-int 10 )))  ; '\'
                                (not (= (str.head x) (char.from-int 13 )))  ; '\'
                                (not (= (str.head x) (char.from-int 11 )))  ; '\'
                                (not (= (str.head x) (char.from-int 0  )))) ; '\x00'
                           (tr1 (str.tail x) (str.tail y)))))

                  ; definition of tr2
                  (or 
                      (and (not (= x "")) (not (= y ""))
                      (and (= (str.head x) (char.from-int 32)) ; ' '
                           (= (str.head y) (char.from-int 32)) ; ' '
                           (tr2 (str.tail x) (str.tail y))))
                      (and (not (= x "")) (not (= y ""))
                      (and (= (str.head x) (char.from-int 09)) ; '\t'
                      	   (= (str.head y) (char.from-int 09)) ; '\t'
                           (tr2 (str.tail x) (str.tail y))))
                      (and (not (= x "")) (not (= y ""))
                      (and (= (str.head x) (char.from-int 10)) ; '\n'
                           (= (str.head y) (char.from-int 10)) ; '\n'
                           (tr2 (str.tail x) (str.tail y))))
                      (and (not (= x "")) (not (= y ""))
                      (and (= (str.head x) (char.from-int 13)) ; '\r'
                           (= (str.head y) (char.from-int 13)) ; '\r'
                           (tr2 (str.tail x) (str.tail y))))
                      (and (not (= x "")) (not (= y ""))
                      (and (= (str.head x) (char.from-int 0 )) ; '\x00'
                           (= (str.head y) (char.from-int 0 )) ; '\x00'
                           (tr2 (str.tail x) (str.tail y))))
                      (and (not (= x "")) (not (= y ""))
                      (and (= (str.head x) (char.from-int 11)) ; '\x0B'
                           (= (str.head y) (char.from-int 11)) ; '\x0B'
                           (tr2 (str.tail x) (str.tail y))))
                      (and (not (= x "")) (not (= y ""))
					  (and (= (str.head x) (str.head y))            ; x <=> y
                           (and (not (= (str.head x) (char.from-int 32 )))  ; '\''
                                (not (= (str.head x) (char.from-int 09 )))  ; '"'
                                (not (= (str.head x) (char.from-int 10 )))  ; '\'
                                (not (= (str.head x) (char.from-int 13 )))  ; '\'
                                (not (= (str.head x) (char.from-int 11 )))  ; '\'
                                (not (= (str.head x) (char.from-int 0  )))) ; '\x00'
                           (tr1 (str.tail x) (str.tail y)))))

                  ; definition of tr2
                  (or (and (= x "") (= y ""))
                      (and (not (= x "")) 
                      (and (= (str.head x) (char.from-int 32)) ; ' '
                           (tr3 (str.tail x) y)))
                      (and (not (= x "")) 
                      (and (= (str.head x) (char.from-int 09)) ; '\t'
                           (tr3 (str.tail x) y)))
                      (and (not (= x ""))
                      (and (= (str.head x) (char.from-int 10)) ; '\n'
                           (tr3 (str.tail x) y)))
                      (and (not (= x ""))
                      (and (= (str.head x) (char.from-int 13 )) ; '\r'
                           (tr3 (str.tail x) y)))
                      (and (not (= x ""))
                      (and (= (str.head x) (char.from-int 0  )) ; '\x00'
                           (tr3 (str.tail x) y)))
                      (and (not (= x ""))
                      (and (= (str.head x) (char.from-int 11 )) ; '\x0B'
                           (tr3 (str.tail x) y))))
                  )
)

(declare-fun prothostpath () String)
(declare-fun prothostpath1 () String)
(declare-fun querfrag () String)
(declare-fun querfrag1 () String)
(declare-fun querfrag11 () String)
(declare-fun querfrag2 () String)
(declare-fun url () String)
(declare-fun url1 () String)
(declare-fun url2 () String)
(declare-fun qmarkpos () Int)
(declare-fun sharppos () Int)

(assert (= prothostpath ""))
(assert (= querfrag ""))
;(assert (trim url url1))
(assert (= qmarkpos (str.indexof url1 "?" 0)))
(assert (= sharppos (str.indexof url1 "#" 0)))
(assert (>= qmarkpos 0))
(assert (= prothostpath1 (str.substr url1 0 qmarkpos)))
(assert (= querfrag1 (str.substr url1 qmarkpos (- (str.len url1) qmarkpos)))) 
(assert (= querfrag2 (str.replaceall querfrag1 "<script>" "")))
(assert (= url2 (str.++ prothostpath1 querfrag2)))
(assert (not (str.in.re querfrag2 (re.++ (re.* re.allchar) (str.to.re "<script>") (re.* re.allchar) ))))

(check-sat)
(get-model)
