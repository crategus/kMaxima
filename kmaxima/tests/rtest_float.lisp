;;;; Created on 2011-10-30 15:09:36

(in-package :kmaxima-tests)

(define-test fpformat
  (assert-equal '(#\0 #\. #\0 #\b #\0 ) (fpformat bigfloatzero))
  (assert-equal '(#\1 #\. #\0 #\b #\0 ) (fpformat bigfloatone))
  (assert-equal '(#\5 #\. #\0 #\b #\- #\1) (fpformat bfhalf))
  (assert-equal '(#\2 #\. #\7 #\1 #\8 #\2 #\8 #\1 #\8 #\2 #\8 #\4 #\5 #\9 #\0
                          #\4 #\5 #\b #\0 )
                (fpformat ($bfloat '$%e)))
  (assert-equal '(#\3 #\. #\1 #\4 #\1 #\5 #\9 #\2 #\6 #\5 #\3 #\5 #\8 #\9 #\7
                          #\9 #\3 #\b #\0 )
                 (fpformat ($bfloat '$%pi)))
  (assert-equal '(#\5 #\. #\7 #\7 #\2 #\1 #\5 #\6 #\6 #\4 #\9 #\0 #\1 #\5 #\3
                          #\2 #\9 #\b #\- #\1 )
                (fpformat ($bfloat '$%gamma))))

(define-test intofp
  (assert-equal '(0 0) (intofp 0))
  (assert-equal '(36028797018963968 1) (intofp 1))
  (assert-equal '(54043195528445952 1) (intofp 1.5))
  (assert-equal '(56593902016227522 2) (intofp '$%pi))
  (assert-equal '(48968212118944587 2) (intofp '$%e))
  (assert-equal '(41592772053807304 0) (intofp '$%gamma))
  (assert-equal '(58295818150454590 1) (intofp '$%phi)))
                
                