;;;; Created on 2011-07-17 12:51:13

(in-package :kmaxima-tests)

(defmacro with-display (&body body)
  `(let ((*mratp* nil)
         (*maxht*     1) (*maxdp*  0) (*width*  0)
         (*height*    0) (*depth*  0) (*level*  0) (*size*    2)
         (*break*     0) (*right*  0) (*lines*  1) (*bkpt*  nil)
         (*bkptwd*    0) (*bkptht* 1) (*bkptdp* 0) (*bkptout* 0)
         (*bkptlevel* 0))
     (progn
       ,@body)))

(define-test display
  (with-display
    (assert-equal '(#\b #\  #\+ #\  #\a)
                  (dimension '((mplus) a b) nil 'mparen 'mparen 0 0)))
  (with-display
    (assert-equal '(#\b #\  #\a)
                  (dimension '((mtimes) a b) nil 'mparen 'mparen 0 0)))
  (with-display
    (assert-equal '((d-hbar 1) (-1 0) (-1 -1 #\b) (0 1 #\a))
                  (dimension '((mquotient) a b) nil 'mparen 'mparen 0 0)))
  (with-display
    (assert-equal '((d-hbar 5) (-5 0)
                  (-4 -1 #\b #\  #\+ #\  #\a) (1 1 #\y #\  #\x))
                  (dimension '((mquotient) ((mtimes) x y) ((mplus) a b))
                             nil 'mparen 'mparen 0 0)))
  )

