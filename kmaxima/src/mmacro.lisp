;;;; Created on 2011-04-07 19:16:26

(in-package :kmaxima)

;;; ----------------------------------------------------------------------------

(defmacro defun-prop (f arg &body body)
  `(setf (get ',(first f) ',(second f)) #'(lambda ,arg ,@body)))

(defmacro defmspec (function . rest)
  `(progn
     (defun-prop (,function mspec) ,@rest)))

;;; ----------------------------------------------------------------------------

(defvar *variable-initial-values* (make-hash-table))

(defmacro defmvar (var &rest val-and-doc)
  (cond ((> (length val-and-doc) 2)
         (setq val-and-doc (list (car val-and-doc) (second val-and-doc)))))
  `(progn
     (unless (gethash ',var *variable-initial-values*)
       (setf (gethash ',var *variable-initial-values*) ,(first val-and-doc)))
     (defvar ,var ,@val-and-doc)))

;;; ----------------------------------------------------------------------------

(defvar errset nil)

(defmacro errset (&rest l)
  `(handler-case (list ,(car l))
     (error (e) (when errset (error e)))))

;;; ----------------------------------------------------------------------------
