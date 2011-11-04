;;;; Created on 2011-10-30 15:09:36

(in-package :kmaxima-tests)

(define-test fpformat
  (assert-equal '(|0| |.| |0| |b| |0|)     (fpformat bigfloatzero))
  (assert-equal '(|1| |.| |0| |b| |0|)     (fpformat bigfloatone))
  (assert-equal '(|5| |.| |0| |b| |-| |1|) (fpformat bfhalf))
  (assert-equal '(|2| |.| |7| |1| |8| |2| |8| |1| |8| |2| |8| |4| |5| |9| |0|
                          |4| |5| |b| |0|)
                (fpformat bigfloat%e))
  (assert-equal '(|3| |.| |1| |4| |1| |5| |9| |2| |6| |5| |3| |5| |8| |9| |7|
                          |9| |3| |b| |0|)
                 (fpformat bigfloat%pi))
  (assert-equal '(|5| |.| |7| |7| |2| |1| |5| |6| |6| |4| |9| |0| |1| |5| |3|
                          |2| |9| |b| |-| |1|)
                (fpformat bigfloat%gamma)))
