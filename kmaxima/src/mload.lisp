;;; ----------------------------------------------------------------------------
;;; mload.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains modified code from:
;;;
;;; Copyright (C) 1984, 1987 William Schelter, University of Texas
;;; Copyright (C) 1982 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;; ----------------------------------------------------------------------------

(in-package :kmaxima)

(defmvar $file_search_lisp   '((mlist simp)))
(defmvar $file_search_maxima '((mlist simp)))
(defmvar $file_search_demo   '((mlist simp)))

(defmvar $file_search_tests
         '((mlist simp)
           "/home/dieter/Lisp/kMaxima/kmaxima/tests/###.{mac,mc}"))

(defmvar $testsuite_files nil)

(defparameter *maxima-testsdir* "/home/dieter/Lisp/kMaxima/kmaxima/tests")

;;; ----------------------------------------------------------------------------

(defun split-string (string bag &optional (start 0) &aux all pos v l)
  (declare (fixnum start) (type string string))
  (loop for i from start below (length string)
        do 
        (setq pos (position (setq v (aref string i)) bag))
        (setq start (+ start 1))
        (cond ((null pos) (push v all))
              (t (if all (loop-finish))))
        finally
        (if all
            (return-from split-string
              (cons (make-array (setq l (length all))
                                :fill-pointer l
                                :adjustable t
                                :initial-contents (nreverse all)
                                :element-type
                                ' #.(array-element-type "ab"))
                    (split-string string bag start))))))

(defun alter-pathname (pathname &rest options)
  (apply 'make-pathname :defaults (pathname pathname) options))

;;; ----------------------------------------------------------------------------

(defun list-variable-bindings (expr &optional str &aux tem)
  (loop for v in (cdr ($listofvars  expr))
    when (member v $values :test #'equal)
    collecting (setq tem `((mequal) ,v ,(meval* v)))
    and
    do (cond (str (format str ",")(mgrind tem str)))))

;;; ----------------------------------------------------------------------------

(defun $filename_merge (&rest file-specs)
  (when (or (null file-specs)
            (cddr file-specs))
    (wna-err '$filename_merge))
  (setq file-specs (mapcar #'macsyma-namestring-sub file-specs))
  (pathname (if (null (cdr file-specs))
                (car file-specs)
                (merge-pathnames (cadr file-specs) (car file-specs)))))

(defun macsyma-namestring-sub (user-object)
  (if (pathnamep user-object)
      user-object
      (let* ((system-object
              (cond ((and (atom user-object)
                          (not (symbolp user-object)))
                     user-object)
                    ((atom user-object)
                     (print-invert-case (fullstrip1 user-object)))
                    ((mlistp user-object)
                     (fullstrip (cdr user-object)))
                    (t
                     (merror "filename_merge: unexpected argument: ~A"
                             user-object))))
             (namestring-try (errset-namestring system-object)))
        (if namestring-try
            (car namestring-try)
            (merror "filename_merge: unexpected argument: ~:M"
                    user-object)))))

(defun errset-namestring (x)
  (let ((*errset* nil))
    (errset (pathname x) nil)))

;;; ----------------------------------------------------------------------------

(defun $file_search (name &optional paths)
  (if (and (symbolp name)
           (member (char (symbol-name name) 0) '(#\$) ))
      (setq name (subseq (print-invert-case name) 1)))
  (if (symbolp name)
      (setf name (string name)))
  (if (probe-file name)
      (return-from $file_search name))
  (unless paths
    (setq paths
          (cons '(mlist simp)
                (append (cdr $file_search_lisp)
                        (cdr $file_search_maxima)
                        (cdr $file_search_demo)))))
  (unless (mlistp paths)
    (merror "file_search: The argument `paths' must be a list."))
  (file-search (string name) (cdr paths)))

(defun file-search (name template)
  (cond ((probe-file name))
        ((and (not (null template))
              (atom template))
         (let ((lis (loop for w in (split-string template "{}")
                          when (null (position #\, w))
                          collect w
                          else
                          collect (split-string w ","))))
           (file-search1 name "" lis)))
        (t
         (let ((temp nil))
           (loop for v in template
                 when (setq temp (file-search name v))
                 do (return temp))))))

(defun file-search1 (name begin lis)
  (cond ((null lis)
         (let ((file (namestring ($filename_merge begin name))))
           (if (probe-file file) file nil)))
        ((atom (car lis))
         (file-search1 name
                           (if begin
                               (concatenate 'string begin (car lis))
                               (car lis))
                           (cdr lis)))
        (t
         (loop for v in (car lis) with tem
               when (setq tem
                          (file-search1 name begin (cons v (cdr lis))))
               do (return tem)))))

;;; ----------------------------------------------------------------------------

(defun lispify-maxima-keyword-options (options &optional valid-keywords)
  (unless (listp options)
    (merror "run_testsuite: Invalid Maxima keyword options: ~A" options))
  (when (every #'(lambda (o)
                   (let ((ok (and (listp o)
                                  (= (length o) 3)
                                  (eq (caar o) 'mequal))))
                     (unless ok
                       (merror
                         "run_testsuite: Badly formed keyword option: ~M"
                         o))
                     ok))
               options)
    (mapcan #'(lambda (o)
                (destructuring-bind (mequal opt val)
                    o
                  (declare (ignore mequal))
                  (if (or (null valid-keywords)
                          (member opt valid-keywords))
                      (flet ((keywordify (x)
                               (intern (subseq (symbol-name x) 1) :keyword)))
                        (list (keywordify opt) val))
                      (merror "run_testsuite: Unrecognized keyword: ~M"
                              opt))))
            options)))

(defun $run_testsuite (&rest options)
  (apply #'run-testsuite
         (lispify-maxima-keyword-options options
                                         '($display_all
                                           $display_known_bugs
                                           $tests $time))))

(defun run-testsuite (&key display_known_bugs display_all tests time)
  (declare (special $file_search_tests))
  (let ((test-file)
        (expected-failures))
    (unless (member display_known_bugs '(t nil))
      (merror "run_testsuite: display_known_bugs must be true or false;~
              found: ~A"
              display_known_bugs))
    (unless (member display_all  '(t nil))
      (merror "run_testsuite: display_all must be true or false; found: ~A"
              display_all))
    (unless (member time '(t nil $all))
      (merror "run_testsuite: time must be true, false, or all; found: ~M"
              time))
    (setq *collect-errors* nil)
    (unless $testsuite_files
      (load (concatenate 'string *maxima-testsdir* "/" "testsuite.lisp")))
    (let ((error-break-file)
          (testresult)
          (tests-to-run (intersect-tests tests))
          (test-count 0)
          (total-count 0)
          (error-count 0))
      (time
       (loop with errs = '() for testentry in tests-to-run
             do
             (if (atom testentry)
                 (progn
                   (setf test-file testentry)
                   (setf expected-failures nil))
                 (progn
                   (setf test-file (second testentry))
                   (setf expected-failures (cddr testentry))))
             (format t "Running tests in ~a: "
                       (if (symbolp test-file)
                           (subseq (print-invert-case test-file) 1)
                           test-file))
             (or (errset
                   (progn
                     (multiple-value-setq (testresult test-count)
                       (test-batch ($file_search test-file $file_search_tests)
                                   expected-failures
                                   :show-expected display_known_bugs
                                   :show-all display_all
                                   :showtime time))
                     (setf testresult (rest testresult))
                     (incf total-count test-count)
                     (when testresult
                       (incf error-count (length (cdr testresult)))
                       (setq errs (append errs (list testresult))))))
                 (progn
                   (setq error-break-file (format nil "~a" test-file))
                   (setq errs
                         (append errs
                                 (list (list error-break-file
                                             "error break"))))
                   (format t "~%Caused an error break: ~a~%" test-file)))
             finally
             (cond ((null errs)
                    (format t
                          "~%~%No unexpected errors found out of ~:D tests.~%"
                            total-count))
                   (t
                    (format t "~%Error summary:~%")
                    (mapcar #'(lambda (x)
                                (let ((s (if (> (length (rest x)) 1) "s" "")))
                                  (format t
                                       "Error~a found in ~a, problem~a:~%~a~%"
                                          s (first x) s (sort (rest x) #'<))))
                            errs)
                    (format t "~&~:D test~P failed out of ~:D total tests.~%"
                            error-count error-count total-count)))))))
  '$done)

(defun intersect-tests (tests)
  (flet ((remove-dollarsign (x)
           (if (symbolp x)
               (subseq (print-invert-case x) 1)
               x)))
    (mapcar #'remove-dollarsign
            (cond (tests
                   (let ((results nil))
                     (dolist (test (mapcar #'remove-dollarsign (cdr tests)))
                       (when (find test (cdr $testsuite_files)
                                   :key #'(lambda (x)
                                            (print-invert-case (if (listp x)
                                                                   (second x)
                                                                   x)))
                                   :test #'string= )
                         (push test results)))
                     (nreverse results)))
                  (t
                   (cdr $testsuite_files))))))

(defun test-batch (filename expected-errors
                            &key (out *standard-output*)
                                 (show-expected nil)
                                 (show-all nil) (showtime nil))
  (let ((result)
        (next-result)
        (next)
        (error-log)
        (all-differences nil)
        ($ratprint nil)
        (strm)
        (*mread-prompt* "")
        (expr)
        (num-problems 0)
        (tmp-output)
        (save-output)
        (i 0)
        (start-run-time 0) (end-run-time 0)
        (start-real-time 0) (end-real-time 0)
        (test-start-run-time 0) (test-end-run-time 0)
        (test-start-real-time 0) (test-end-real-time 0))
    (cond (*collect-errors*
           (setq error-log
                 (if (streamp *collect-errors*)
                     *collect-errors*
                     (handler-case
                       (open (alter-pathname filename :type "ERR")
                             :direction
                             :output
                             :if-exists
                             :supersede)
                       #-gcl (file-error () nil)
                       #+gcl (cl::error () nil))))
           (when error-log
             (format t "~%batch: write error log to ~a" error-log)
             (format error-log
                     "~%/* Maxima error log from tests in ~A"
                     filename)
             (format error-log " */~2%"))))
    (unwind-protect
      (progn
        (setq strm (open filename :direction :input))
        (setq start-real-time (get-internal-real-time))
        (setq start-run-time (get-internal-run-time))
        (while (not (eq 'eof (setq expr (mread strm 'eof))))
          (incf num-problems)
          (incf i)
          (setf tmp-output (make-string-output-stream))
          (setf save-output *standard-output*)
          (setf *standard-output* tmp-output)
          (unwind-protect
            (progn
              (setq test-start-run-time (get-internal-run-time))
              (setq test-start-real-time (get-internal-real-time))
              (setq result
                    (maxima-toplevel-eval `(($errcatch) ,(third expr))))
              (setq result (if (alike1 result '((mlist)))
                               'error-catch
                               (second result)))
              (setq test-end-run-time (get-internal-run-time))
              (setq test-end-real-time (get-internal-real-time))
              (setq $% result))
            (setf *standard-output* save-output))
          (setq next (mread strm 'eof))
          (if (eq next 'eof)
              (merror "batch: missing expected result in test script."))
          (setq next-result (third next))
          (let* ((correct (batch-equal-check next-result result))
                 (expected-error (member i expected-errors))
                 (pass (or correct expected-error)))
            (when (or show-all (not pass) (and correct expected-error)
                      (and expected-error show-expected))
              (format out
                      "~%********************** Problem ~A ***************"
                      i)
              (format out "~%Input:~%")
              (mdisplay (third expr))
              (format out "~%~%Result:~%")
              (format out "~a" (get-output-stream-string tmp-output))
              (mdisplay $%)
              (when (eq showtime '$all)
                (format out "~%Time:  ~,3F sec (~,3F elapsed)"
                        (float (/ (- test-end-run-time test-start-run-time)
                                  internal-time-units-per-second))
                        (float (/ (- test-end-run-time test-start-run-time)
                                  internal-time-units-per-second)))))
            (cond ((and correct expected-error)
                   (format t
                           "~%... Which was correct, but was expected ~
                           to be wrong due to a known bug in~% Maxima.~%"))
                  (correct
                   (if show-all (format t "~%... Which was correct.~%")))
                  ((and (not correct) expected-error)
                   (if (or show-all show-expected)
                       (progn
                         (format t
                                 "~%This is a known error in Maxima. ~
                                 The correct result is:~%")
                         (mdisplay next-result))))
                  (t
                   (format t "~%This differed from the expected result:~%")
                   (push i all-differences)
                   (mdisplay next-result)
                   (cond ((and *collect-errors* error-log)
                          (format error-log "/* Problem ~A */~%" i)
                          (mgrind (third expr) error-log)
                          (list-variable-bindings (third expr) error-log)
                          (format error-log ";~%")
                          (format error-log "/* Erroneous Result?:~%")
                          (mgrind result error-log) (format error-log " */ ")
                          (terpri error-log)
                          (format error-log "/* Expected result: */~%")
                          (mgrind next-result error-log)
                          (format error-log ";~%~%"))))))))
      (close strm))
    (setq end-run-time (get-internal-run-time))
    (setq end-real-time (get-internal-real-time))
    (cond (error-log
           (or (streamp *collect-errors*)
               (close error-log))))
    (let
      ((expected-errors-trailer
         (if (or (null expected-errors) (= (length expected-errors) 0))
             ""
             (format nil
                     " (not counting ~a expected errors)"
                     (length expected-errors))))
       (time (if showtime
                 (format nil "   using ~,3F seconds (~,3F elapsed).~%"
                         (float (/ (- end-run-time start-run-time)
                                   internal-time-units-per-second))
                         (float (/ (- end-real-time start-real-time)
                                   internal-time-units-per-second)))
                 "")))
      (cond ((null all-differences)
             (format t "~a/~a tests passed~a~%~A"
                     num-problems num-problems
                     expected-errors-trailer
                     time)
             (values '((mlist)) num-problems))
            (t
             (format t
                     "~%~a/~a tests passed~a~%~A"
                     (- num-problems (length all-differences))
                     num-problems expected-errors-trailer
                     time)
             (let ((s (if (> (length all-differences) 1) "s" "")))
               (format t
                       "~%The following ~A problem~A failed: ~A~%"
                       (length all-differences)
                       s
                       (reverse all-differences)))
             (values `((mlist) ,filename ,@(reverse all-differences))
                     num-problems))))))

;;; ----------------------------------------------------------------------------

(defun batch-equal-check (expected result)
  (let ((answer (catch 'macsyma-quit (simple-equal-p expected result))))
    (if (eql answer 'maxima-error) nil answer)))

(defun simple-equal-p (f g)
  (approx-alike (simplifya f nil) (simplifya g nil)))

(defun approx-alike (f g)
  (cond ((floatp f)
         (and (floatp g)
              ($float_approx_equal f g)))
        ((bigfloatp f)
         (and (bigfloatp g)
              ($bfloat_approx_equal f g)))
        ((atom f) (and (atom g) (equal f g)))
        ((moperatorp f 'lambda)
         (and (moperatorp g 'lambda)
              (approx-alike-list (mapcar #'(lambda (s) (simplifya s nil))
                                         (margs f))
                                 (mapcar #'(lambda (s) (simplifya s nil))
                                         (margs g)))))
        ((arrayp f)
         (and (arrayp g) (approx-alike ($listarray f) ($listarray g))))
        ((hash-table-p f)
         (and (hash-table-p g) (approx-alike ($listarray f) ($listarray g))))
        ((moperatorp f 'mquote)
         (approx-alike (second f) g))
        ((and (consp f)
              (consp (car f))
              (consp g)
              (consp (car g))
              (or (approx-alike (mop f) (mop g))
                  (and (symbolp (mop f)) (symbolp (mop g))
                       (approx-alike ($nounify (mop f)) ($nounify (mop g)))))
              (approx-alike-list (margs f) (margs g))))
        (t nil)))

(defun approx-alike-list (p q)
  (cond ((null p) (null q))
        ((null q) (null p))
        (t
         (and (approx-alike (first p) (first q))
              (approx-alike-list (rest p) (rest q))))))

(defconstant flonum-epsilon double-float-epsilon)
(defmvar $float_approx_equal_tolerance (* 16 flonum-epsilon))

(defun $float_approx_equal (a b)
  (setq a (if (floatp a) a ($float a)))
  (setq b (if (floatp b) b ($float b)))
  (and (floatp a)
       (floatp b)
       (<= (abs (- a b))
           (* $float_approx_equal_tolerance
              (min (expt 2 
                         (- (second (multiple-value-list (decode-float a)))
                            1))
                   (expt 2
                         (- (second (multiple-value-list (decode-float b)))
                            1)))))))

(defun $bfloat_approx_equal (a b)
  (setq a (if (bigfloatp a) a ($bfloat a)))
  (setq b (if (bigfloatp b) b ($bfloat b)))
  (let ((m) (bits))
    (and (bigfloatp a)
         (bigfloatp b)
         (setq bits (min (third (first a)) (third (first b))))
         (setq m (mul 32
                      (expt 2 (- bits))
                      (min (expt 2 (- (car (last a)) 1))
                           (expt 2 (- (car (last b)) 1)))))
         (setq m (if (rationalp m)
                     (div (numerator m) (denominator m))
                     m))
         (setq m (fpsub (cdr ($bfloat m))
                        (fpabs (fpsub (cdr a) (cdr b)))))
         (or (eql (car m) 0)
             (fpposp m)))))

;;; ----------------------------------------------------------------------------
