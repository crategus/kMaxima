;;;; 2010-12-30 16:21:25

(in-package :kmaxima)

;;; ----------------------------------------------------------------------------

(defvar *maxima-version* 0.1)

(defun maxima-banner ()
  (format t "~&kMaxima ~a~%" *maxima-version*)
  (format t "using Lisp ~a ~a~%" (lisp-implementation-type)
                                 (lisp-implementation-version))
  (format t "Distributed under the GNU Public License. ~
             See the file COPYING.~%")
  (format t "Dedicated to the memory of William Schelter.~%"))

;;; ----------------------------------------------------------------------------

(defun bye ()
  (sb-ext:quit))

(defun $quit ()
  (throw 'quit-to-lisp 0))

(defun used-area (&optional unused)
  (declare (ignore unused))
  (sb-ext:get-bytes-consed))

;;; ----------------------------------------------------------------------------

(defvar *prompt-prefix* "")
(defvar *prompt-suffix* "")

(defmvar $prompt "_")

(defun main-prompt ()
  (declare (special *display-labels-p* $inchar $linenum))
  (if *display-labels-p*
      (let ((*print-circle* nil))
        (format nil "~A(~A~D) ~A"
                *prompt-prefix*
                (print-invert-case (stripdollar $inchar))
                $linenum
                *prompt-suffix*))
      ""))

(defun break-prompt ()
  (let ((*print-circle* nil))
    (format nil "~A~A~A"
            *prompt-prefix*
            (print-invert-case (stripdollar $prompt))
            *prompt-suffix*)))

;;; ----------------------------------------------------------------------------

(defvar *linelabel* nil)

(defmvar $linenum 0)
(defmvar $inchar   '$%i)
(defmvar $outchar  '$%o)
(defmvar $linechar '$%t)

(defmvar $nolabels nil)
(defmvar $labels (list '(mlist simp)))

(defun createlabel (x num)
  (intern (format nil "~a~d" x num)))

(defun checklabel (x)
  (not (or $nolabels
           (= $linenum 0)
           (boundp (createlabel x $linenum)))))

(defun makelabel (x)
  (declare (special *linelabel*))
  (setq *linelabel* (createlabel x $linenum))
  (unless $nolabels
    (when (or (null (cdr $labels))
              (when (member *linelabel* (cddr $labels) :test #'equal)
                (setf $labels
                      (delete *linelabel* $labels :count 1 :test #'eq))
                t)
              (not (eq *linelabel* (cadr $labels))))
      (setq $labels (cons (car $labels) (cons *linelabel* (cdr $labels))))))
  *linelabel*)

(defun getfirstcharlabel (x)
  (let ((c (char (symbol-name x) 1)))
    (if (char= c #\%)
        (char (symbol-name x) 2)
        c)))

(defun getlabels (x)
  (do ((l (cdr $labels) (cdr l))
       (ch (getfirstcharlabel x))
       (acc))
      ((null l) (reverse acc))
    (if (char= (getfirstcharlabel (car l)) ch)
        (push (car l) acc))))

(defun getlabels2 (n1 n2 &optional (flag nil))
  (do ((i n1 (1+ i))
       (acc)
       (l (if flag
              (list $inchar)
              (list $inchar $outchar $linechar))))
      ((> i n2) (reverse acc))
    (do ((l l (cdr l))
         (z))
        ((null l))
      (if (boundp (setq z (createlabel (first l) i)))
          (push z acc)))))

(defun $labels (x)
  (cons '(mlist simp) (getlabels x)))

(defun $%th (x)
  (prog (l outchar)
    (if (or (not (fixnump x)) (= x 0))
        (merror "Improper argument to ~:M:~%~M" '$%th x))
    (if (> x 0) (setq x (- x)))
    (if (cdr $labels)
        (setq l (cddr $labels)
              outchar (getfirstcharlabel $outchar)))
  loop
    (if (null l) (merror "Improper call to %th"))
    (if (and (char= (getfirstcharlabel (car l)) outchar)
             (= (setq x (1+ x)) 0))
        (return (meval (car l))))
    (setq l (cdr l))
    (go loop)))

;;; ----------------------------------------------------------------------------

(proclaim '(ftype (function (*) *) meval))

(defun mevalargs (args)
  (mapcar #'meval args))

;;; ----------------------------------------------------------------------------

(defun meval (form &aux u)
  (cond 
    ((atom form)
     (cond ((not (symbolp form))
            form)
           ((not (boundp form))
            form)
           (t (symbol-value form))))
    ((consp (car form))
     (let ((op (caar form)))
       (cond ((mfunctionp op)
              (apply op (mevalargs (cdr form))))
             ((setq u (getprop op 'mspec))
              (apply u (cons form nil)))
             ((macro-function op)
              (eval (cons op (cdr form))))
             (t
              (cons (cons op nil) (mevalargs (cdr form)))))))
    (t (eval form))))

;;; ----------------------------------------------------------------------------

(defvar *general-display-prefix* "")

(defmvar $showtime nil)

(defun maxima-toplevel-loop (input-stream mode)
  (declare (special $% $_ $__))
  (when (eql mode :demo)
    (format t
    "~%At the '~A' prompt, type ';' and <enter> to get next demonstration.~&"
            (print-invert-case (stripdollar $prompt))))
  (catch 'abort-demo
    (do ((form)
         (time) (etime) (area)
         (eof (list nil))
         (i-tag)
         (o-tag))
        (nil)
      (catch 'return-from-debugger
        (when (or (not (checklabel $inchar))
                  (not (checklabel $outchar)))
          (incf $linenum))
        (setq i-tag (makelabel $inchar))
        (let ((*mread-prompt* (if mode nil (main-prompt))))
          (setq form (maxima-toplevel-read input-stream nil eof)))
        (format t "~a" *general-display-prefix*)
        (if (eq form eof) (return '$done))
        (setq $__ (caddr form))
        (unless $nolabels (set i-tag $__))
        (when mode (linear-display `((mlabel) ,i-tag , $__)))
        (setq time (get-internal-run-time)
              etime (get-internal-real-time))
        (setq area (used-area))
        (setq $% (maxima-toplevel-eval $__))
        (setq time (/ (float (- (get-internal-run-time) time))
                      internal-time-units-per-second)
              etime (/ (float (- (get-internal-real-time) etime))
                       internal-time-units-per-second))
        (setq o-tag (makelabel $outchar))
        (unless $nolabels (set o-tag $%))
        (setq $_ $__)
        (when $showtime
          (format t "Evaluation took ~,4F seconds (~,4F elapsed)" time etime)
          (let ((total-bytes (- (used-area) area)))
            (cond ((> total-bytes (* 1024 1024))
                   (format t " using ~,3F MB.~%"
                             (/ total-bytes (* 1024.0 1024.0))))
                  ((> total-bytes 1024)
                   (format t " using ~,3F KB.~%" (/ total-bytes 1024.0)))
                  (t
                   (format t " using ~:D bytes.~%" total-bytes)))))
        (unless $nolabels
          (putprop '$% (cons time etime) 'time)
          (putprop o-tag (cons time  etime) 'time))
        (if (eq (caar form) 'displayinput)
            (linear-display `((mlabel) ,o-tag ,$%)))
        (when (eq mode ':demo)
          (princ (break-prompt))
          (force-output)
          (let (quitting)
            (do ((char)) (nil)
              (case (setq char (read-char *terminal-io*))
                ((#\page)
                 (fresh-line)
                 (princ (break-prompt))
                 (force-output))
                ((#\?)
                 (format t
                      "Pausing. Type a ';' and <enter> to continue demo.~%"))
                ((#\space #\; #\n #\e #\x #\t))
                ((#\newline)
                 (if quitting (throw 'abort-demo nil) (return nil)))
                (t (setq quitting t))))))
        (when mode
          (do ((char)) (())
            (setq char (read-char input-stream nil nil))
            (when (null char)
              (throw 'macsyma-quit nil))
            (unless (member char '(#\space #\newline #\return #\tab))
              (unread-char char input-stream)
              (return nil))))))))

;;; ----------------------------------------------------------------------------

(defvar *need-prompt* t)

(defun maxima-toplevel-read (stream eof-p eof)
  (let ((mprompt *mread-prompt*)
        (*mread-prompt* "")
        ch)
    (if (and *need-prompt* (> (length mprompt) 0))
        (progn
          (fresh-line *standard-output*)
          (princ mprompt *standard-output*)
          (force-output *standard-output*)
          (setf *prompt-on-read-hang* nil))
        (progn
          (setf *prompt-on-read-hang* t)
          (setf *read-hang-prompt* mprompt)))
    (tagbody
     top
      (setq ch (read-char stream eof-p eof))
      (cond ((or (eql ch #\newline)
                 (eql ch #\return))
             (go top))
            ((eq ch eof)
             (return-from maxima-toplevel-read eof)))
      (unread-char ch stream))
    (cond
      ((eql #\? ch)
       (read-char stream)
       (let ((next (peek-char nil stream nil)))
         (cond
           ((member next '(#\space #\tab #\!))
            (let ((line (string-trim '(#\space #\tab #\; #\$ )
                                     (subseq (read-line stream eof-p eof)
                                             1))))
              `((displayinput) nil (($describe) ,line $exact))))
           ((equal next #\?)
            (let ((line (string-trim '(#\space #\tab #\; #\$ )
                                     (subseq (read-line stream eof-p eof) 
                                             1))))
              `((displayinput) nil (($describe) ,line $inexact))))
           (t
            (mread (make-concatenated-stream (make-string-input-stream "?")
                                             stream)
                   eof)))))
      (t
       (let ((result (mread stream eof))
             (next-char (read-char-no-hang stream eof-p eof)))
         (cond ((or (eql next-char nil)
                    (equal next-char '(nil)))
                (setf *need-prompt* t))
               ((member next-char '(#\newline #\return))
                (setf *need-prompt* t))
               (t
                (setf *need-prompt* nil)
                (unread-char next-char stream)))
         result)))))

;;; ----------------------------------------------------------------------------

(defun simplifya (form flag)
  (declare (ignore flag))
  form)

(defun maxima-toplevel-eval (form)
  (simplifya (meval form) nil))

;;; ----------------------------------------------------------------------------

(defvar *maxima-quiet* nil)
(defvar *maxima-epilog* "")

(let ((maxima-started nil))
  (defun maxima-toplevel (&optional (input-stream *standard-input*) mode)
    (let ((*package* (find-package :kmaxima)))
      (if maxima-started
          (format t "Maxima restarted.~%")
          (progn
            (if (not *maxima-quiet*) (maxima-banner))
            (setq maxima-started t)))
      (catch 'quit-maxima-toplevel
             (in-package :kmaxima)
             (loop
               (catch 'maxima-continue
                      (maxima-toplevel-loop input-stream mode)
                      (format t *maxima-epilog*)
                      (bye)))))))

;;; ----------------------------------------------------------------------------

(defun cl-user::run ()
  (in-package :kmaxima)
  (let ((input-stream *standard-input*)
        (mode nil))
    (catch 'quit-to-lisp
           (loop
            (with-simple-restart (kmaxima "Return to kMaxima top level.")
              (maxima-toplevel input-stream mode))))))

(import 'cl-user::run)

;;; ----------------------------------------------------------------------------

(defun $writefile (x)
  (let ((msg (dribble x)))
    (if msg
        (format t "~&~A~&" msg))
    '$done))

(defun $closefile ()
  (let ((msg (dribble)))
    (if msg
        (format t "~&~A~&" msg)))
  '$done)

;;; ----------------------------------------------------------------------------

(defun merror (message &rest args)
  (format t message args)
  (format t "~& -- an error. To debug this try: debugmode(true);~%")
  (throw 'maxima-continue 'maxima-error))

;;; ----------------------------------------------------------------------------

(defvar *values* nil)
(defvar *options* nil)

(defmvar $optionset nil)

(defun mset (x y)
  (cond ((symbolp x)
         (let ((f (getprop x 'assign)))
           (if (and f (or (not (eq x y))
                          (eq f 'neverset)))
               (if (eq (funcall f x y) 'munbindp)
                   (return-from mset nil))))
         (cond ((not (boundp x))
                (push x *values*))
               ((and (not (eq x y))
                     (boundp x)
                     (not (member x *values*)))
                (if $optionset
                    (format t "assignment: assigning to option ~A~%" x))
                (push x *options*)))
         (return-from mset (setf (symbol-value x) y)))
        (t (merror "assignment: cannot assign to ~A~%" x))))

;;; ----------------------------------------------------------------------------

(defvar *munbindp* nil)

(defun mseterror (var val)
  (declare (special *munbindp*))
  (if *munbindp*
      'munbindp
      (merror "assignment: cannot assign ~a to ~a" val var)))

;;; ----------------------------------------------------------------------------

(defun neverset (var val)
  (mseterror var val))

(defun boolset (x y)
  (if (not (member y '(t nil $false $true)))
      (mseterror x y)))

(defun shadowset (var val)
  (mset (get var 'shadowvar) val))

(defun shadowboolset (var val)
  (if (not (member val '(t nil $false $true)))
      (mseterror var val)
      (mset (get var 'shadowvar) val)))

;;; ----------------------------------------------------------------------------

(defun $values ()
  (cons '(mlist simp) (copy-list *values*)))

(defun $options ()
  (cons '(mlist simp) (copy-list *options*)))

;;; ----------------------------------------------------------------------------

(defmvar $numer nil)
(defmvar $float nil)

;;; ----------------------------------------------------------------------------

(defprop $optionset boolset assign)

(defprop $numer shadowboolset assign)
(defprop $numer $float shadowvar)

(defprop $%pi neverset assign)
(defprop $%i neverset assign)
(defprop $%e neverset assign)
(defprop $%phi neverset assign)
(defprop $%gamma neverset assign)

;;; ----------------------------------------------------------------------------

(defmspec mquote (form)
  (cadr form))

(defmspec msetq (l)
  (mset (cadr l) (meval (caddr l))))

;;; ----------------------------------------------------------------------------

(defun reset1 (args)
  (declare (special *variable-initial-values*))
  (labels ((maybe-reset (key val)
             (let ((reset nil))
               (when (and (boundp key)
                          ;; equalp must be generalized for Maxima forms.
                          (not (equalp (symbol-value key) val)))
                 (setq reset key)
                 (let ((*munbindp* t)    ; no error, when reseting
                       ($optionset nil)) ; no message, when reseting
                   (declare (special *munbindp* $optionset))
                   (meval `((msetq) ,key ((mquote) ,val)))))
               reset)))
    (let ((actually-reset nil))
      (if args
        (mapcar
          #'(lambda (key)
              (multiple-value-bind (val found-p)
                  (gethash key *variable-initial-values*)
                (if found-p
                    (if (maybe-reset key val)
                        (push key actually-reset)))))
          args)
        (maphash
          #'(lambda (key val)
              (if (maybe-reset key val)
                  (push key actually-reset)))
          *variable-initial-values*))
      (cons '(mlist) (nreverse actually-reset)))))

(defmspec $reset (l)
  (reset1 (cdr l)))

;;; ----------------------------------------------------------------------------
