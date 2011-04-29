;;;; Created on 2011-01-22 19:49:56

(in-package :kmaxima)

(defvar *display-labels-p* t)

(defun linear-display (form)
  (declare (special *chrps* *display-labels-p*))
  (fresh-line *standard-output*)
  (cond ((not (atom form))
         (cond ((eq (caar form) 'mlabel)
                (setq *chrps* 0)
                (cond ((and (cadr form) *display-labels-p*)
                       (princ "(")
                       (setq *chrps*
                             (+  3 (length (mgrind (cadr form) nil))))
                       (princ ") ")))
                (mprint (msize (caddr form) nil nil 'mparen 'mparen)
                        *standard-output*))
               ((eq (caar form) 'mtext)
                (do ((form (cdr form) (cdr form))
                     (fortranp))
                    ((null form))
                  (setq fortranp (atom (car form)))
                  (mgrind (car form) *standard-output*)))
               (t
                (mgrind form *standard-output*))))
        (t
         (mgrind form *standard-output*)))
  (terpri))
