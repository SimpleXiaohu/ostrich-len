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
                      (and (= (seq-head x) (_ bv32 8)) ; ' '
                           (trim (seq-tail x) y))
                      (and (= (seq-head x) (_ bv09 8)) ; '\t'
                           (trim (seq-tail x) y))
                      (and (= (seq-head x) (_ bv10 8)) ; '\n'
                           (trim (seq-tail x) y))
                      (and (= (seq-head x) (_ bv13  8)) ; '\r'
                           (trim (seq-tail x) y))
                      (and (= (seq-head x) (_ bv0   8)) ; '\x00'
                           (trim (seq-tail x) y))
                      (and (= (seq-head x) (_ bv11  8)) ; '\x0B'
                           (trim (seq-tail x) y))
					  (and (= (seq-head x) (seq-head y))            ; x <=> y
                           (and (not (= (seq-head x) (_ bv32  8)))  ; '\''
                                (not (= (seq-head x) (_ bv09  8)))  ; '"'
                                (not (= (seq-head x) (_ bv10  8)))  ; '\'
                                (not (= (seq-head x) (_ bv13  8)))  ; '\'
                                (not (= (seq-head x) (_ bv11  8)))  ; '\'
                                (not (= (seq-head x) (_ bv0   8)))) ; '\x00'
                           (tr1 (seq-tail x) (seq-tail y))))

                  ; definition of tr1
                  (or (and (= x "") (= y ""))
                      (and (= (seq-head x) (_ bv32 8)) ; ' '
                           (= (seq-head y) (_ bv32 8)) ; ' '
                           (tr2 (seq-tail x) (seq-tail y)))
                      (and (= (seq-head x) (_ bv09 8)) ; '\t'
                      	   (= (seq-head y) (_ bv09 8)) ; '\t'
                           (tr2 (seq-tail x) (seq-tail y)))
                      (and (= (seq-head x) (_ bv10 8)) ; '\n'
                           (= (seq-head y) (_ bv10 8)) ; '\n'
                           (tr2 (seq-tail x) (seq-tail y)))
                      (and (= (seq-head x) (_ bv13 8)) ; '\r'
                           (= (seq-head y) (_ bv13 8)) ; '\r'
                           (tr2 (seq-tail x) (seq-tail y)))
                      (and (= (seq-head x) (_ bv0  8)) ; '\x00'
                           (= (seq-head y) (_ bv0  8)) ; '\x00'
                           (tr2 (seq-tail x) (seq-tail y)))
                      (and (= (seq-head x) (_ bv11 8)) ; '\x0B'
                           (= (seq-head y) (_ bv11 8)) ; '\x0B'
                           (tr2 (seq-tail x) (seq-tail y)))
                      (and (= (seq-head x) (_ bv32 8)) ; ' '
                           (tr3 (seq-tail x) y))
                      (and (= (seq-head x) (_ bv09 8)) ; '\t'
                           (tr3 (seq-tail x) y))
                      (and (= (seq-head x) (_ bv10 8)) ; '\n'
                           (tr3 (seq-tail x) y))
                      (and (= (seq-head x) (_ bv13  8)) ; '\r'
                           (tr3 (seq-tail x) y))
                      (and (= (seq-head x) (_ bv0   8)) ; '\x00'
                           (tr3 (seq-tail x) y))
                      (and (= (seq-head x) (_ bv11  8)) ; '\x0B'
                           (tr3 (seq-tail x) y))
					  (and (= (seq-head x) (seq-head y))            ; x <=> y
                           (and (not (= (seq-head x) (_ bv32  8)))  ; '\''
                                (not (= (seq-head x) (_ bv09  8)))  ; '"'
                                (not (= (seq-head x) (_ bv10  8)))  ; '\'
                                (not (= (seq-head x) (_ bv13  8)))  ; '\'
                                (not (= (seq-head x) (_ bv11  8)))  ; '\'
                                (not (= (seq-head x) (_ bv0   8)))) ; '\x00'
                           (tr1 (seq-tail x) (seq-tail y))))

                  ; definition of tr2
                  (or (and (= (seq-head x) (_ bv32 8)) ; ' '
                           (= (seq-head y) (_ bv32 8)) ; ' '
                           (tr2 (seq-tail x) (seq-tail y)))
                      (and (= (seq-head x) (_ bv09 8)) ; '\t'
                      	   (= (seq-head y) (_ bv09 8)) ; '\t'
                           (tr2 (seq-tail x) (seq-tail y)))
                      (and (= (seq-head x) (_ bv10 8)) ; '\n'
                           (= (seq-head y) (_ bv10 8)) ; '\n'
                           (tr2 (seq-tail x) (seq-tail y)))
                      (and (= (seq-head x) (_ bv13 8)) ; '\r'
                           (= (seq-head y) (_ bv13 8)) ; '\r'
                           (tr2 (seq-tail x) (seq-tail y)))
                      (and (= (seq-head x) (_ bv0  8)) ; '\x00'
                           (= (seq-head y) (_ bv0  8)) ; '\x00'
                           (tr2 (seq-tail x) (seq-tail y)))
                      (and (= (seq-head x) (_ bv11 8)) ; '\x0B'
                           (= (seq-head y) (_ bv11 8)) ; '\x0B'
                           (tr2 (seq-tail x) (seq-tail y)))
					  (and (= (seq-head x) (seq-head y))            ; x <=> y
                           (and (not (= (seq-head x) (_ bv32  8)))  ; '\''
                                (not (= (seq-head x) (_ bv09  8)))  ; '"'
                                (not (= (seq-head x) (_ bv10  8)))  ; '\'
                                (not (= (seq-head x) (_ bv13  8)))  ; '\'
                                (not (= (seq-head x) (_ bv11  8)))  ; '\'
                                (not (= (seq-head x) (_ bv0   8)))) ; '\x00'
                           (tr1 (seq-tail x) (seq-tail y))))

                  ; definition of tr2
                  (or (and (= x "") (= y ""))
                      (and (= (seq-head x) (_ bv32 8)) ; ' '
                           (tr3 (seq-tail x) y))
                      (and (= (seq-head x) (_ bv09 8)) ; '\t'
                           (tr3 (seq-tail x) y))
                      (and (= (seq-head x) (_ bv10 8)) ; '\n'
                           (tr3 (seq-tail x) y))
                      (and (= (seq-head x) (_ bv13  8)) ; '\r'
                           (tr3 (seq-tail x) y))
                      (and (= (seq-head x) (_ bv0   8)) ; '\x00'
                           (tr3 (seq-tail x) y))
                      (and (= (seq-head x) (_ bv11  8)) ; '\x0B'
                           (tr3 (seq-tail x) y)))
                  )
)

(declare-fun prothostpath () String)
(declare-fun prothostpath1 () String)
(declare-fun querfrag () String)
(declare-fun querfrag1 () String)
(declare-fun querfrag2 () String)
(declare-fun url () String)
(declare-fun url1 () String)
(declare-fun url2 () String)
(declare-fun qmarkpos () Int)
(declare-fun sharppos () Int)

(assert (= prothostpath ""))
(assert (= querfrag ""))
(assert (trim url url1))
(assert (= qmarkpos (str.indexof url1 "?" 0)))
(assert (= sharppos (str.indexof url1 "#" 0)))
(assert (< qmarkpos 0))
(assert (< sharppos 0))
(assert (= prothostpath1 url1))
(assert (= querfrag1 (str.replaceall querfrag "<script>" "")))
(assert (= url2 (str.++ prothostpath querfrag1)))
(assert (str.in.re querfrag1 (re.++ (re.* re.allchar) (str.to.re "<script>") (re.* re.allchar) )))

(check-sat)
(get-model)
