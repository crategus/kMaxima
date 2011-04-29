;;; ----------------------------------------------------------------------------
;;; parser.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This file contains modified code from:
;;;
;;; Copyright (C) 1984, 1987 William Schelter, University of Texas
;;; Copyright (C) 1981 Massachusetts Institute of Technology
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

;;; ----------------------------------------------------------------------------

(defprop $true T alias)

;;; ----------------------------------------------------------------------------

(defvar *parse-window* nil)
(defvar *parse-window-length* 25)
(defvar *parse-stream* nil)
(defvar *parse-tyi* nil)
(defvar *parse-stream-eof* -1)

(defvar *prompt-on-read-hang* nil)
(defvar *read-hang-prompt* "")

(defvar *current-line-info* nil)

(defvar *mopl* nil)

(defvar *mread-prompt* nil)
(defvar *mread-eof-obj* nil)

;;; ----------------------------------------------------------------------------

(defconstant flonum-exponent-marker #\D)

(defvar *whitespaces* '(#\tab #\space #\linefeed #\return #\page #\newline))
(defvar *exponent-chars* '(#\E #\e #\F #\f #\B #\b #\D #\d #\S #\s ))

;;; ----------------------------------------------------------------------------

(define-initial-symbols
    |+| |-| |*| |^| |<| |=| |>| |(| |)| |[| |]| |,|
    |:| |!| |#| |'| |$| |;|
    ;; Two character
    |**| |^^| |:=| |::| |<=| |>=| |''|
    ;; Three character
    |::=|
    )

;;; ----------------------------------------------------------------------------

(defstruct instream
  stream
  (line 0 :type fixnum)
  name)

(let ((stream-list nil))
  (defun get-instream (str)
    (or (dolist (v stream-list)
          (cond ((eq str (instream-stream v))
                 (return v))))
        (let (name errset)
          (errset (setq name (namestring str)))
          (car (push (make-instream :stream str :name name) stream-list)))))

  (defun newline (str)
    (incf (instream-line (get-instream str)))
    (values))

  (defun find-stream (stream)
     (dolist (v stream-list)
       (cond ((eq stream (instream-stream v))
              (return v))))))

;;; ----------------------------------------------------------------------------

(defun add-lineinfo (lis)
  (if (or (atom lis)
          (and (eq *parse-window* *standard-input*)
               (not (find-stream *parse-stream*))))
      lis
      (let* ((st (get-instream *parse-stream*))
             (n (instream-line st))
             (nam (instream-name st)))
        (or nam (return-from add-lineinfo lis))
        (setq *current-line-info*
              (cond ((eq (cadr *current-line-info*) nam)
                     (cond ((eql (car *current-line-info*) n)
                            *current-line-info*)
                           (t  (cons n (cdr *current-line-info*)))))
                    (t (list n nam 'src))))
        (cond ((null (cdr lis))
               (list (car lis) *current-line-info*))
              (t (append lis (list *current-line-info*)))))))

;;; ----------------------------------------------------------------------------

(let ((previous-tyi #\a))
  (defun tyi (&optional (stream *standard-input*) eof)
    (labels ((tyi-raw ()
               (let ((ch (read-char-no-hang stream nil eof)))
                 (if ch
                     ch
                     (progn
                       (when (and *prompt-on-read-hang* *read-hang-prompt*)
                         (princ *read-hang-prompt*)
                         (force-output *standard-output*))
                       (read-char stream nil eof)))))
            (backslash-check (ch)
              (if (eq previous-tyi #\\ )
                  (progn (setq previous-tyi #\a) ch)
                  (setq previous-tyi
                        (if (eq ch #\\ )
                            (let ((next-char (peek-char nil stream nil eof)))
                              (if (or (eq next-char #\newline)
                                      (eq next-char #\return))
                                  (eat-continuations ch)
                                  ch))
                            ch))))
            (eat-continuations (ch)
              (setq ch (tyi-raw))
              (do ()
                  ((not (or (eq ch #\newline) (eq ch #\return))))
                (let ((next-char (peek-char nil stream nil eof)))
                  (if (and (eq ch #\return) (eq next-char #\newline))
                      (tyi-raw)))
                (setq ch (tyi-raw))
                (let ((next-char (peek-char nil stream nil eof)))
                  (if (and (eq ch #\\ )
                           (or (eq next-char #\return)
                               (eq next-char #\newline)))
                      (setq ch (tyi-raw))
                      (return-from eat-continuations ch))))
              ch))
    (let ((ch (tyi-raw)))
      (if (eq ch eof)
          ch
          (backslash-check ch))))))

;;; ----------------------------------------------------------------------------

(defun parse-tyi-init (stream eof)
  (or *parse-window*
      (progn
        (setq *parse-window* (make-list *parse-window-length*))
        (nconc *parse-window* *parse-window*)))
  (let ((tem (tyi stream eof)))
    (setf (car *parse-window*) tem
          *parse-window* (cdr *parse-window*))
    (if (eql tem #\newline)
        (newline stream))
    tem))

(defun parse-tyi ()
  (let ((tem *parse-tyi*))
    (cond ((null tem)
           (parse-tyi-init *parse-stream* *parse-stream-eof*))
          ((atom tem)
           (setq *parse-tyi* nil)
           tem)
          (t
           (setq *parse-tyi* (cdr tem))
           (car tem)))))

(defun parse-tyipeek ()
  (let ((tem *parse-tyi*))
    (cond ((null tem)
           (setq *parse-tyi*
                 (parse-tyi-init *parse-stream* *parse-stream-eof*)))
          ((atom tem) tem)
          (t (car tem)))))

(defun unparse-tyi (ch)
  (let ((tem *parse-tyi*))
    (if (null tem)
        (setq *parse-tyi* ch)
        (setq *parse-tyi* (cons ch tem)))))

;;; ----------------------------------------------------------------------------

(defun mopstrip (x)
  (cond ((null x) 'false)
        ((or (eq x t) (eq x 't)) 'true)
        ((numberp x) x)
        ((symbolp x)
         (or (getprop x 'reversealias)
             (let ((name (symbol-name x)))
               (if (member (char name 0) '(#\$ #\%) :test #'char=)
                   (subseq name 1)
                   name))))
        (t x)))

;;; ----------------------------------------------------------------------------

(defun mread-synerr (format-string &rest l)
  (let (tem
        errset
        (file "stdin"))
    (errset (setq tem (file-position *parse-stream*))
            (setq file (namestring *parse-stream*)))
    (when tem
      (format t "~%~a:~a:" file tem))
    (format t "incorrect syntax: ")
    (apply 'format t format-string
           (mapcar #'(lambda (x)
                       (if (symbolp x) (print-invert-case x) x))
                   l))
    (cond ((eql *parse-stream* *standard-input*)
           (let ((n *parse-window-length*)
                 some ch)
             (loop for i from (1- n) downto (- n 20)
                   while (setq ch (nth i *parse-window*))
                   do
                   (cond ((eql ch #\newline)
                          (push #\n some)
                          (push #\\ some))
                         ((eql ch #\tab)
                          (push #\t some)
                          (push #\\ some))
                         (t (push ch some))))
             (format t "~%~{~c~}~%~vt^" some (- (length some) 2))
             (read-line *parse-stream* nil nil))))
    (terpri)
    (throw 'maxima-continue t)))

;;; ----------------------------------------------------------------------------

(defun parse-err ()
  (mread-synerr "Syntax error"))

(defun parse-bug-err (op)
  (mread-synerr
    "Parser bug in ~A. Please report this to the Maxima maintainers,~
   ~%including the characters you just typed which caused the error. Thanks."
    (mopstrip op)))

(defun delim-err (op)
  (mread-synerr "Illegal use of delimiter ~A" (mopstrip op)))

(defun erb-err (op l)
  (declare (ignore l))
  (mread-synerr "Too many ~A's" (mopstrip op)))

(defun premterm-err (op)
  (mread-synerr "Premature termination of input at ~A." (mopstrip op)))

;;; ----------------------------------------------------------------------------

(defvar *scan-buffered-token* (list nil)
  "Put-back buffer for scanner, a state-variable of the reader")

(defun peek-one-token (&optional (eof-p nil) (eof nil))
  (cond ((car *scan-buffered-token*)
         (cdr *scan-buffered-token*))
        (t
         (rplacd *scan-buffered-token* (scan-one-token eof-p eof))
         (cdr (rplaca *scan-buffered-token* t)))))

(defun scan-one-token (&optional (eof-p nil) (eof nil) &aux test)
  (cond ((car *scan-buffered-token*)
         (rplaca *scan-buffered-token* nil)
         (cdr *scan-buffered-token*))
        ((scan-operator-token *maxima-operators*))
        ((eql (setq test (parse-tyipeek)) *parse-stream-eof*)
         (parse-tyi)
         (if eof-p
             eof
             (merror "parser: end of file while scanning expression.")))
        ((eql test #\/ )
         (parse-tyi)
         (cond ((char= (parse-tyipeek) #\* )
                (parse-tyi)
                (gobble-comment)
                (scan-one-token eof-p eof))
               (t '$/)))
        ((eql test #\. )
         (parse-tyi)
         (if (digit-char-p (parse-tyipeek) 10)
             (scan-number-after-dot (list (list #\. ) nil))
             '|$.|))
        ((eql test #\" )
         (parse-tyi)
         (scan-string))
        ((eql test #\? )
         (parse-tyi)
         (cond ((char= (parse-tyipeek) #\" )
                (parse-tyi)
                (scan-string))
               ((char= (parse-tyipeek) #\: )
                (scan-keyword-token))
               (t (scan-lisp-token))))
        ((digit-char-p test 10)
         (scan-number-before-dot nil))
        (t (scan-maxima-token))))

;;; ----------------------------------------------------------------------------

(defun gobble-comment ()
  (prog (c depth)
    (setq depth 1)
  read
    (setq c (parse-tyipeek))
    (parse-tyi)
    (if (= depth 0) (return t))
    (cond ((and (numberp c) (< c 0))
           (merror "parser: end of file in comment."))
          ((char= c #\* )
           (cond ((char= (parse-tyipeek) #\/ )
                  (decf depth)
                  (parse-tyi)
                  (if (= depth 0) (return t))
                  (go read))))
          ((char= c #\/ )
           (cond ((char= (parse-tyipeek) #\* )
                  (incf depth) 
                  (parse-tyi)
                  (go read)))))
        (go read)))

;;; ----------------------------------------------------------------------------

(defun scan-operator-token (obj)
  (do ((ch (parse-tyipeek) (parse-tyipeek)))
      ((not (member ch 
                    '(#\newline #\tab #\space #\linefeed #\return #\page))))
    (parse-tyi))
  (scan-operator-token-aux obj))

(defun scan-operator-token-aux (obj)
  (labels ((parser-assoc (ch lis)
             (do ((v lis (cdr v)))
                 ((null v))
               (cond ((consp (car v))
                      (if (eql (caar v) ch) (return (car v))))
                     ((eql (car v) ch)
                      (return v))))))
    (let* ((ch (parse-tyipeek))
           (lis (if (eql ch *parse-stream-eof*)
                    nil
                    (parser-assoc ch obj)))
           result)
      (cond
        ((null lis) nil)
        (t
         (parse-tyi)
         (cond 
           ((atom (cadr lis))
            (setq result (scan-operator-token-aux (list (cdr lis)))))
           ((null (cddr lis))
            (setq result
                  (and (eql (car (cadr lis)) 'ans)
                       (or (not (alphabetp (cadr (exploden (cadadr lis)))))
                           (member (parse-tyipeek) *whitespaces*))
                       (cadr (cadr lis)))))
           (t
            (let ((res (and (eql (car (cadr lis)) 'ans) (cadadr lis)))
                  (token (scan-operator-token-aux (cddr lis))))
              (setq result 
                    (or token 
                        res
                        (scan-operator-token-aux (list (cadr lis))))))))
         (or result (unparse-tyi ch))
         result)))))

;;; ----------------------------------------------------------------------------

(defun scan-maxima-token ()
  (getalias (implode (cons '#\$ (scan-token t)))))

(defun scan-lisp-token ()
  (let ((charlist (scan-token nil)))
    (if charlist
        (implode charlist)
        (mread-synerr "Lisp symbol expected."))))

(defun scan-keyword-token ()
  (let ((charlist (cdr (scan-token nil))))
    (if charlist
        (let ((*package* (find-package :keyword)))
          (implode charlist))
        (mread-synerr "Lisp keyword expected."))))

(defun scan-token (flag)
  (do ((c (parse-tyipeek) (parse-tyipeek))
       (l () (cons c l)))
      ((and flag
            (not (or (digit-char-p c (max 10 *read-base*))
                     (alphabetp c)
                     (char= c #\\ ))))
       (nreverse (or l (list (parse-tyi)))))
    (when (char= (parse-tyi) #\\ )
      (setq c (parse-tyi)))
    (setq flag t)))

;;; ----------------------------------------------------------------------------

(defun scan-string (&optional init)
  (let ((buf (make-array 50 :element-type '#.(array-element-type "a")
                            :fill-pointer 0 :adjustable t)))
    (when init
      (vector-push-extend init buf))
    (do ((c (parse-tyipeek) (parse-tyipeek)))
        ((cond ((eql c -1))
               ((char= c #\")
                (parse-tyi) t))
         (copy-seq buf))
      (if (char= (parse-tyi) #\\ )
          (setq c (parse-tyi)))
          (vector-push-extend c buf))))

;;; ----------------------------------------------------------------------------

(defun readlist (lis)
  (read-from-string (coerce lis 'string)))

(defun make-number (data)
  (setq data (nreverse data))
  (let ((marker (car (nth 3 data))))
    (unless (eql marker flonum-exponent-marker)
      (when (member marker '(#\E #\F #\S #\D #\L ))
        (setf (nth 3 data) (list flonum-exponent-marker)))))
  (readlist (apply #'append data)))

(defun scan-digits (data continuation? continuation &optional exponent-p)
  (do ((c (parse-tyipeek) (parse-tyipeek))
       (l () (cons c l)))
      ((not (and (characterp c) (digit-char-p c (max 10 *read-base*))))
       (cond ((member c continuation?)
              (funcall continuation
                       (list* (list (char-upcase (parse-tyi)))
                              (nreverse l)
                              data)))
             ((and (null l) exponent-p)
              (merror "parser: incomplete number; missing exponent?"))
             (t
              (make-number (cons (nreverse l) data)))))
    (parse-tyi)))

(defun scan-number-exponent (data)
  (push (list (if (or (char= (parse-tyipeek) #\+ )
                      (char= (parse-tyipeek) #\- ))
                  (parse-tyi)
                  #\+ ))
        data)
  (scan-digits data nil nil t))

(defun scan-number-rest (data)
  (let ((c (caar data)))
    (cond ((member c '(#\.))
           (scan-number-after-dot data))
          ((member c *exponent-chars*)
           (setf data (push (list #\. ) (rest data)))
           (push (list #\0 ) data)
           (push (list c ) data)
           (scan-number-exponent data)))))

(defun scan-number-before-dot (data)
  (scan-digits data (push #\. *exponent-chars*) #'scan-number-rest))

(defun scan-number-after-dot (data)
  (scan-digits data *exponent-chars* #'scan-number-exponent))

;;; ----------------------------------------------------------------------------

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro def-nud-equiv (op equiv)
    (list 'putprop (list 'quote op) (list 'function equiv) (list 'quote 'nud)))

  (defmacro nud-propl () ''(nud))

  (defmacro def-nud-fun (op-name op-l . body)
    (list* 'defun-prop (list* op-name 'nud 'nil) op-l body))

  (defmacro def-led-equiv (op equiv)
    (list 'putprop (list 'quote op) (list 'function equiv) (list 'quote 'led)))

  (defmacro led-propl () ''(led))

  (defmacro def-led-fun (op-name op-l . body)
    (list* 'defun-prop (list* op-name 'led 'nil) op-l body)))

;;; ----------------------------------------------------------------------------

(defmacro def-operatorp ()
  `(defun operatorp (lex)
     (and (symbolp lex) (getpropl lex '(,@(nud-propl) ,@(led-propl))))))

(def-operatorp)

(defmacro def-operatorp1 ()
  `(defun operatorp1 (lex)
     (and (symbolp lex) (getpropl lex '(lbp rbp ,@(nud-propl) ,@(led-propl))))))

(def-operatorp1)

;;; ----------------------------------------------------------------------------

(defun nud-call (op)
  (let ((tem (getprop op 'nud))
        res)
    (setq res
          (if (null tem)
              (if (operatorp op)
                  (mread-synerr "~A is not a prefix operator" (mopstrip op))
                  (cons '$any op))
              (funcall tem op)))
    res))

(defun led-call (op l)
  (let ((tem (getprop op 'led))
        res)
    (setq res
          (if (null tem)
              (mread-synerr "~A is not an infix operator" (mopstrip op))
              (funcall tem op l)))
    res))

;;; ----------------------------------------------------------------------------

(defmacro def-nud ((op . lbp-rbp) bvl . body)
  (let ((lbp (nth 0 lbp-rbp))
        (rbp (nth 1 lbp-rbp)))
    `(progn
       'compile
       ,(make-parser-fun-def op 'nud bvl body)
       (set-lbp-and-rbp ',op ',lbp ',rbp))))

(defun set-lbp-and-rbp (op lbp rbp)
  (cond 
    ((not (consp op))
     (let ((existing-lbp (get op 'lbp))
           (existing-rbp (get op 'rbp)))
       (cond ((not lbp))
             ((not existing-lbp)
              (putprop op lbp 'lbp))
             ((not (equal existing-lbp lbp))
              (merror "Incompatible LBP's defined for this operator ~a" op)))
       (cond ((not rbp))
             ((not existing-rbp)
              (putprop op rbp 'rbp))
             ((not (equal existing-rbp rbp))
              (merror "Incompatible RBP's defined for this operator ~a"
                      op)))))
    (t
     (mapcar #'(lambda (x) (set-lbp-and-rbp x lbp rbp)) op))))

(defmacro def-led((op . lbp-rbp) bvl . body)
  (let ((lbp (nth 0 lbp-rbp))
        (rbp (nth 1 lbp-rbp)))
    `(progn 
       'compile
       ,(make-parser-fun-def  op 'led bvl body)
       (set-lbp-and-rbp ',op ',lbp ',rbp))))

;;; ----------------------------------------------------------------------------

(defmacro def-collisions (op &rest alist)
  (let ((keys (do ((i 1 (ash i 1))
                   (lis  alist (cdr lis))
                   (nl () (cons (cons (caar lis) i) nl)))
                  ((null lis) nl))))
    `(progn 
       'compile
       (defprop ,op ,(let nil (copy-tree keys )) keys)
       ,@(mapcar 
           #'(lambda (data)
               `(defprop 
                  ,(car data)
                  ,(do ((i 0 (logior i (cdr (assoc (car lis)
                                                   keys :test #'eq))))
                        (lis (cdr data) (cdr lis)))
                       ((null lis) i))
                  ,op))
          alist))))

(defun collision-lookup (op active-bitmask key-bitmask)
  (let ((result (logand active-bitmask key-bitmask)))
    (if (not (zerop result))
        (do ((l (get op 'keys) (cdr l)))
            ((null l) (parse-bug-err 'collision-check))
          (if (not (zerop (logand result (cdar l))))
              (return (caar l)))))))

(defun collision-check (op active-bitmask key)
  (let ((key-bitmask (get key op)))
    (if (not key-bitmask)
        (mread-synerr "~A is an unknown keyword in a ~A statement."
                      (mopstrip key) (mopstrip op)))
    (let ((collision (collision-lookup op active-bitmask key-bitmask)))
      (if collision
          (if (eq collision key)
              (mread-synerr "This ~A's ~A slot is already filled."
                            (mopstrip op)
                            (mopstrip key))
              (mread-synerr "A ~A cannot have a ~A with a ~A field."
                            (mopstrip op)
                            (mopstrip key)
                            (mopstrip collision))))
      (logior (cdr (assoc key (get op 'keys) :test #'eq)) active-bitmask))))

;;; ----------------------------------------------------------------------------

(defun lbp (lex)
  (cond ((getprop lex 'lbp)) (t 200)))

(defmacro def-lbp (sym val)
  `(defprop ,sym ,val lbp))

(defun rbp (lex)
  (cond ((getprop lex 'rbp)) (t 200)))

(defmacro def-rbp (sym val) 
  `(defprop ,sym ,val rbp))

(defmacro def-match (x m) 
  `(defprop ,x ,m match))

;;; ----------------------------------------------------------------------------

(defun lpos (op) (cond ((getprop op 'lpos)) (t '$any)))
(defun rpos (op) (cond ((getprop op 'rpos)) (t '$any)))
(defun pos  (op) (cond ((getprop op 'pos))  (t '$any)))

(defmacro def-pos  (op pos) `(defprop ,op ,pos  pos))
(defmacro def-rpos (op pos) `(defprop ,op ,pos rpos))
(defmacro def-lpos (op pos) `(defprop ,op ,pos lpos))

;;; ----------------------------------------------------------------------------

(defun mheader (op)
  (add-lineinfo (or (getprop op 'mheader) (list op))))

(defmacro def-mheader (op header) `(defprop ,op ,header mheader))

;;; ----------------------------------------------------------------------------

(defprop $any    "untyped"   english)
(defprop $clause "logical"   english)
(defprop $expr   "algebraic" english)

;;; ----------------------------------------------------------------------------

(defmacro first-c () '(peek-one-token))
(defmacro pop-c   () '(scan-one-token))

;;; ----------------------------------------------------------------------------

(defun mread (&rest read-args)
  (progn
    (when *mread-prompt*
      (and *parse-window* 
           (setf (car *parse-window*) nil
                 *parse-window* (cdr *parse-window*)))
      (princ *mread-prompt*)
      (force-output))
    (apply 'mread-raw read-args)))

(defun mread-raw (*parse-stream* &optional *mread-eof-obj*)
  (let ((*scan-buffered-token* (list nil))
        *parse-tyi*)
    (if (eq *scan-buffered-token* (peek-one-token t *scan-buffered-token*))
        *mread-eof-obj*
        (do ((labels ())
             (input (parse '$any 0) (parse '$any 0)))
            (nil)
          (case (first-c)
            ((|$;| |$$|
              )
             (setf *current-line-info* nil)
             (return (list (mheader (pop-c))
                           (if labels
                               (cons (mheader '|$[| ) (nreverse labels)))
                           input)))
            ((|$&&|)
             (pop-c)
             (if (symbolp input)
                 (push input labels)
                 (mread-synerr "Invalid && tag. Tag must be a symbol")))
            (t
             (parse-bug-err 'mread-raw)))))))

;;; ----------------------------------------------------------------------------

(defun parse (mode rbp)
  (do ((left (nud-call (pop-c))         ; Envoke the null left denotation
       (led-call (pop-c) left)))        ;  and keep calling LED ops as needed
      ((>= rbp (lbp (first-c)))         ; Until next op lbp too low
       (convert left mode))))           ;  in which case, return stuff seen

(defun parse-prefix (op)
  (list (pos op)                        ; Operator mode
        (mheader op)                    ; Standard Macsyma expression header
        (parse (rpos op) (rbp op))))    ; Convert single argument for use

(defun parse-postfix (op l)
  (list (pos op)                        ; Operator's mode
        (mheader op)                    ; Standard Macsyma expression header
        (convert l (lpos op))))         ; Convert single argument for use

(defun parse-infix (op l)
  (list (pos op)                        ; Operator's mode
        (mheader op)                    ; Standard Macsyma expression header
        (convert l (lpos op))           ; Convert arg1 for immediate use
        (parse (rpos op) (rbp op))))    ; Look for an arg2 

(defun parse-nary (op l)
  (list* (pos op)                           ; Operator's mode
         (mheader op)                       ; Normal Macsyma operator header
         (convert l (lpos op))              ; Check type-match of arg1 
         (prsnary op (lpos op) (lbp op))))  ; Search for other args

(defun parse-matchfix (op)
  (list* (pos op)                           ; Operator's mode
         (mheader op)                       ; Normal Macsyma operator header
         (prsmatch (and (symbolp op)
                        (get op 'match))
                   (lpos op))))             ; Search for matchfixed forms

(defun parse-nofix (op)
  (list (pos op) (mheader op)))

(defun prsnary (op mode rbp)
  (do ((nl (list (parse mode rbp))          ; Get at least one form
           (cons (parse mode rbp) nl)))     ;  and keep getting forms
      ((not (eq op (first-c)))              ; until a parse pops on a new op
       (nreverse nl))                       ;  at which time return forms
      (pop-c)))                             ; otherwise pop op

(defun prsmatch (match mode)
  (cond ((eq match (first-c)) (pop-c) nil)
        (t
         (do ((nl (list (parse mode 10))
                  (cons (parse mode 10) nl)))
             ((eq match (first-c))
              (pop-c)
              (nreverse nl))
           (if (eq '|$,| (first-c))
               (pop-c)
               (mread-synerr "Missing ~A"
                             (mopstrip match)))))))

(defun convert (item mode)
  (if (or (eq mode (car item))                    ; If modes match exactly
          (eq '$any mode)                         ;    or target is $ANY
          (eq '$any (car item)))                  ;    or input is $ANY
      (cdr item)                                  ;  then return expression
      (mread-synerr "Found ~A expression where ~A expression expected"
                    (get (car item) 'english)
                    (get mode       'english))))

;;; ----------------------------------------------------------------------------

(def-nud-equiv |$]| delim-err)
(def-led-equiv |$]| erb-err)
(def-lbp       |$]| 5)

(def-nud-equiv |$[| parse-matchfix)
(def-match     |$[| |$]|)
(def-lbp       |$[| 200)
;No RBP
(def-mheader   |$[| (mlist))
(def-pos       |$[| $any)
(def-lpos      |$[| $any)
;No RPOS

(def-led (|$[| 200) (op left)
  (setq left (convert left '$any))
  (if (numberp left) (parse-err))
  (let ((header (if (atom left)
                    (add-lineinfo (list (amperchk left) 'array))
                    (add-lineinfo '(mqapply array))))
        (right (prsmatch '|$]| '$any)))
    (cond ((null right)
           (mread-synerr "No subscripts given"))
          ((atom left)
           (setq right (cons header right))
           (cons '$any (getalias right)))
          (t
           (cons '$any (cons header (cons left right)))))))

;;; ----------------------------------------------------------------------------

(def-nud-equiv |$)| delim-err)
(def-led-equiv |$)| erb-err)
(def-lbp       |$)| 5)

(def-mheader   |$(| (mprogn))

(def-nud (|$(| 200) (op)
  (let ((right) (hdr (mheader '|$(|)))
    (cond ((eq '|$)| (first-c)) (parse-err))
          ((or (null (setq right (prsmatch '|$)| '$any)))
               (cdr right))
           (cons '$any (cons hdr right)))
          (t (cons '$any (car right))))))

(def-led (|$(| 200) (op left)
  (setq left (convert left '$any))
  (if (numberp left) (parse-err))
  (let ((hdr (and (atom left)(mheader (amperchk left))))
        (r (prsmatch '|$)| '$any)))
    (cons '$any
          (cond ((atom left)
                 (cons hdr r))
                (t
                 (cons '(mqapply) (cons left r)))))))

;;; ----------------------------------------------------------------------------

(def-mheader |$'| (mquote))

(def-nud (|$'|) (op)
  (let (right)
    (cond ((eq '|$(| (first-c))
           (list '$any (mheader '|$'|) (parse '$any 190)))
          ((or (atom (setq right (parse '$any 190.)))
               (member (caar right)
                       '(mquote mlist mprog mprogn lambda) :test #'eq))
           (list '$any (mheader '|$'|) right))
          ((eq 'mqapply (caar right))
           (cond ((eq (caaadr right) 'lambda)
                  (list '$any (mheader '|$'|) right))
                 (t
                  (rplaca (cdr right)
                          (cons (cons ($nounify (caaadr right))
                                      (cdaadr right))
                                (cdadr right)))
                  (cons '$any right))))
           (t 
            (cons '$any (cons (cons ($nounify (caar right)) (cdar right))
                              (cdr right)))))))

(def-nud (|$''|) (op)
  (let (right)
    (cons '$any
          (cond ((eq '|$(| (first-c)) (meval (parse '$any 190)))
                ((atom (setq right (parse '$any 190))) (meval right))
                ((eq 'mqapply (caar right))
                 (rplaca (cdr right)
                         (cons (cons ($verbify (caaadr right))
                                     (cdaadr right))
                               (cdadr right)))
                 right)
                (t
                 (cons (cons ($verbify (caar right)) (cdar right))
                       (cdr right)))))))
  
;;; ----------------------------------------------------------------------------

(def-led-equiv |$:| parse-infix)
(def-lbp       |$:| 180)
(def-rbp       |$:|  20)
(def-pos       |$:| $any)
(def-rpos      |$:| $any)
(def-lpos      |$:| $any)
(def-mheader   |$:| (msetq))

(def-led-equiv |$::| parse-infix)
(def-lbp       |$::| 180)
(def-rbp       |$::|  20)
(def-pos       |$::| $any)
(def-rpos      |$::| $any)
(def-lpos      |$::| $any)
(def-mheader   |$::| (mset))

(def-led-equiv |$:=| parse-infix)
(def-lbp       |$:=| 180)
(def-rbp       |$:=|  20)
(def-pos       |$:=| $any)
(def-rpos      |$:=| $any)
(def-lpos      |$:=| $any)
(def-mheader   |$:=| (mdefine))

(def-led-equiv |$::=| parse-infix)
(def-lbp       |$::=| 180)
(def-rbp       |$::=|  20)
(def-pos       |$::=| $any)
(def-rpos      |$::=| $any)
(def-lpos      |$::=| $any)
(def-mheader   |$::=| (mdefmacro))

(def-led-equiv |$!| parse-postfix)
(def-lbp       |$!| 160)
;No RBP
(def-pos       |$!| $expr)
(def-lpos      |$!| $expr)
;No RPOS
(def-mheader   |$!| (mfactorial))

(def-mheader   |$!!| ($genfact))

(def-led (|$!!| 160.) (op left)
  (list '$expr
        (mheader '$!!)
        (convert left '$expr)
        (list (mheader '$/) (convert left '$expr) 2)
        2))

(def-lbp       |$^| 140)
(def-rbp       |$^| 139)
(def-pos       |$^| $expr)
(def-lpos      |$^| $expr)
(def-rpos      |$^| $expr)
(def-mheader   |$^| (mexpt))

(def-led ((|$^| |$^^|)) (op left)
  (cons '$expr
        (getalias (list (mheader op)
                        (convert left (lpos op))
                        (parse (rpos op) (rbp op))))))

(mapc #'(lambda (prop) ; Make $** like $^
          (let ((propval (get '$^ prop)))
            (if propval (putprop '$** propval prop))))
      '(lbp rbp pos rpos lpos mheader))

(inherit-propl  '$** '$^ (led-propl))

(def-lbp       |$^^| 140)
(def-rbp       |$^^| 139)
(def-pos       |$^^| $expr)
(def-lpos      |$^^| $expr)
(def-rpos      |$^^| $expr)
(def-mheader   |$^^| (mncexpt))

(def-led-equiv |$.| parse-infix)
(def-lbp       |$.| 130)
(def-rbp       |$.| 129)
(def-pos       |$.| $expr)
(def-lpos      |$.| $expr)
(def-rpos      |$.| $expr)
(def-mheader   |$.| (mnctimes))

(def-led-equiv |$*| parse-nary)
(def-lbp       |$*| 120)
;RBP not needed
(def-pos       |$*| $expr)
;RPOS not needed
(def-lpos      |$*| $expr)
(def-mheader   |$*| (mtimes))

(def-led-equiv $/  parse-infix)
(def-lbp       $/  120)
(def-rbp       $/  120)
(def-pos       $/  $expr)
(def-rpos      $/  $expr)
(def-lpos      $/  $expr)
(def-mheader   $/  (mquotient))

(def-nud-equiv |$+| parse-prefix)
(def-lbp       |$+| 100)
(def-rbp       |$+| 134) ; Value increased from 100 to 134 (DK 02/2010).
(def-pos       |$+| $expr)
(def-rpos      |$+| $expr)
;LPOS not needed
(def-mheader   |$+| (mplus))

(def-led ((|$+| |$-|) 100) (op left)
  (setq left (convert left '$expr))
  (do ((nl (list (if (eq op '$-)
                     (list (mheader '$-) (parse '$expr 100))
                     (parse '$expr 100))
                 left)
           (cons (parse '$expr 100) nl)))
      ((not (member (first-c) '($+ $-) :test #'eq))
       (list* '$expr (mheader '$+) (nreverse nl)))
    (if (eq (first-c) '$+) (pop-c))))

(def-nud-equiv |$-| parse-prefix)
(def-lbp       |$-| 100)
(def-rbp       |$-| 134)
(def-pos       |$-| $expr)
(def-rpos      |$-| $expr)
;LPOS not needed
(def-mheader   |$-| (mminus))

(def-led-equiv |$=| parse-infix)
(def-lbp       |$=| 80)
(def-rbp       |$=| 80)
(def-pos       |$=| $clause)
(def-rpos      |$=| $expr)
(def-lpos      |$=| $expr)
(def-mheader   |$=| (mequal))

(def-led-equiv |$>| parse-infix)
(def-lbp       |$>| 80)
(def-rbp       |$>| 80)
(def-pos       |$>| $clause)
(def-rpos      |$>| $expr)
(def-lpos      |$>| $expr)
(def-mheader   |$>| (mgreaterp))

(def-led-equiv |$>=| parse-infix)
(def-lbp       |$>=| 80)
(def-rbp       |$>=| 80)
(def-pos       |$>=| $clause)
(def-rpos      |$>=| $expr)
(def-lpos      |$>=| $expr)
(def-mheader   |$>=| (mgeqp))

(def-led-equiv |$<| parse-infix)
(def-lbp       |$<| 80)
(def-rbp       |$<| 80)
(def-pos       |$<| $clause)
(def-rpos      |$<| $expr)
(def-lpos      |$<| $expr)
(def-mheader   |$<| (mlessp))

(def-led-equiv |$<=| parse-infix)
(def-lbp       |$<=| 80)
(def-rbp       |$<=| 80)
(def-pos       |$<=| $clause)
(def-rpos      |$<=| $expr)
(def-lpos      |$<=| $expr)
(def-mheader   |$<=| (mleqp))

(def-nud-equiv $not parse-prefix)
;LBP not needed
(def-rbp       $not 70)
(def-pos       $not $clause)
(def-rpos      $not $clause)
(def-lpos      $not $clause)
(def-mheader   $not (mnot))

(def-led-equiv $and parse-nary)
(def-lbp       $and 65)
;RBP not needed
(def-pos       $and $clause)
;RPOS not needed
(def-lpos      $and $clause)
(def-mheader   $and (mand))

(def-led-equiv $or parse-nary)
(def-lbp       $or 60)
;RBP not needed
(def-pos       $or $clause)
;RPOS not needed
(def-lpos      $or $clause)
(def-mheader   $or (mor))

(def-led-equiv |$,| parse-nary)
(def-lbp       |$,| 10)
;RBP not needed
(def-pos       |$,| $any)
;RPOS not needed
(def-lpos      |$,| $any)
(def-mheader   |$,| ($ev))

(def-nud-equiv $then delim-err)
(def-lbp $then 5)
(def-rbp $then 25)

(def-nud-equiv $else delim-err)
(def-lbp $else 5)
(def-rbp $else 25)

(def-nud-equiv $elseif delim-err)
(def-lbp  $elseif 5)
(def-rbp  $elseif 45)
(def-pos  $elseif $any)
(def-rpos $elseif $clause)

;No LBP - Default as high as possible
(def-rbp     $if 45)
(def-pos     $if $any)
(def-rpos    $if $clause)
;No LPOS
(def-mheader $if (mcond))

(def-nud ($if) (op)
  (list* (pos op)
         (mheader op)
         (parse-condition op)))

(defun parse-condition (op)
  (list* (parse (rpos op) (rbp op))
         (if (eq (first-c) '$then)
             (parse '$any (rbp (pop-c)))
             (mread-synerr "Missing `then'"))
         (case (first-c)
           (($else)   (list t (parse '$any (rbp (pop-c)))))
           (($elseif) (parse-condition (pop-c)))
           (t ; Note: $false instead of () makes DISPLA suppress display!
            (list t '$false)))))

;;; ----------------------------------------------------------------------------

(defmacro make-mdo () '(list (list 'mdo) nil nil nil nil nil nil nil))

(defmacro mdo-op (x)     `(car (car ,x)))

(defmacro mdo-for (x)    `(second ,x))
(defmacro mdo-from (x)   `(third ,x))
(defmacro mdo-step (x)   `(fourth ,x))
(defmacro mdo-next (x)   `(fifth ,x))
(defmacro mdo-thru (x)   `(sixth ,x))
(defmacro mdo-unless (x) `(seventh ,x))
(defmacro mdo-body (x)   `(eighth ,x))

;;; ----------------------------------------------------------------------------

(def-mheader $do (mdo))

(defun parse-$do (lex &aux (left (make-mdo)))
  (setf (car left) (mheader 'mdo))
  (do ((op lex (pop-c))  (active-bitmask 0))
      (nil)
    (if (eq op '|$:|) (setq op '$from))
    (setq active-bitmask (collision-check '$do active-bitmask op))
    (let ((data (parse (rpos op) (rbp op))))
      (case op
        ($do   (setf (mdo-body left) data) (return (cons '$any left)))
        ($for  (setf (mdo-for  left) data))
        ($from (setf (mdo-from left) data))
        ($in   (setf (mdo-op   left) 'mdoin)
               (setf (mdo-from left) data))
        ($step (setf (mdo-step left) data))
        ($next (setf (mdo-next left) data))
        ($thru (setf (mdo-thru left) data))
        (($unless $while)
               (if (eq op '$while)
                   (setq data (list (mheader '$not) data)))
               (setf (mdo-unless left)
                     (if (null (mdo-unless left))
                         data
                         (list (mheader '$or) data (mdo-unless left)))))
        (t (parse-bug-err '$do))))))

(def-lbp $for    25)
(def-lbp $from   25)
(def-lbp $step   25)
(def-lbp $next   25)
(def-lbp $thru   25)
(def-lbp $unless 25)
(def-lbp $while  25)
(def-lbp $do     25)

(def-nud-equiv $for    parse-$do)
(def-nud-equiv $from   parse-$do)
(def-nud-equiv $step   parse-$do)
(def-nud-equiv $next   parse-$do)
(def-nud-equiv $thru   parse-$do)
(def-nud-equiv $unless parse-$do)
(def-nud-equiv $while  parse-$do)
(def-nud-equiv $do     parse-$do)

(def-rbp $do      25)
(def-rbp $for    200)
(def-rbp $from    95)
(def-rbp $in      95)
(def-rbp $step    95)
(def-rbp $next    45)
(def-rbp $thru    95)
(def-rbp $unless  45)
(def-rbp $while   45)

(def-rpos $do     $any)
(def-rpos $for    $any)
(def-rpos $from   $any)
(def-rpos $step   $expr)
(def-rpos $next   $any)
(def-rpos $thru   $expr)
(def-rpos $unless $clause)
(def-rpos $while  $clause)

(def-collisions $do
  ($do     . ())
  ($for    . ($for))
  ($from   . ($in $from))
  ($in     . ($in $from $step $next))
  ($step   . ($in       $step $next))
  ($next   . ($in	$step $next))
  ($thru   . ($in $thru)) ;$IN didn't used to get checked for
  ($unless . ())
  ($while  . ()))

(def-mheader   |$$| (nodisplayinput))
(def-nud-equiv |$$| premterm-err)
(def-lbp       |$$| -1)
;No RBP, POS, RPOS, RBP, or MHEADER

(def-mheader   |$;| (displayinput))
(def-nud-equiv |$;| premterm-err)
(def-lbp       |$;| -1)
;No RBP, POS, RPOS, RBP, or MHEADER

(def-nud-equiv  |$&&| delim-err)
(def-lbp        |$&&| -1)

(def-led-equiv  |$#| parse-infix)
(def-lbp        |$#| 80)
(def-rbp        |$#| 80)
(def-pos        |$#| $clause)
(def-rpos       |$#| $expr)
(def-lpos       |$#| $expr)
(def-mheader    |$#| (mnotequal))

;;; ----------------------------------------------------------------------------

(defun $prefix (operator &optional (rbp 180) (rpos '$any) (pos '$any))
  (def-operator operator 
                pos () () rbp rpos () t
                '(nud . parse-prefix) 'msize-prefix 'dimension-prefix ())
  operator)

(defun $postfix (operator &optional (lbp 180) (lpos '$any) (pos '$any))
  (def-operator operator pos lbp lpos () () t ()
                '(led . parse-postfix) 'msize-postfix 'dimension-postfix ())
  operator)

(defun $infix (operator &optional (lbp 180) (rbp 180) (lpos '$any) 
                                  (rpos '$any) (pos  '$any))
  (def-operator operator pos lbp lpos rbp rpos t t
                '(led . parse-infix) 'msize-infix 'dimension-infix ())
  operator)

(defun $nary (operator &optional (bp 180) (argpos '$any) (pos '$any))
  (def-operator operator pos bp  argpos bp () t t
                '(led . parse-nary) 'msize-nary 'dimension-nary ())
  operator)

(defun $matchfix (operator match &optional (argpos '$any) (pos '$any))
  (def-operator operator pos () argpos () () () ()
                '(nud . parse-matchfix)
                'msize-matchfix
                'dimension-match match)
  operator)

(defun $nofix (operator &optional (pos '$any))
  (def-operator operator pos () () () () () ()
                '(nud . parse-nofix) 'msize-nofix 'dimension-nofix ())
  operator)

;;; ----------------------------------------------------------------------------

(defun def-operator (op pos lbp lpos rbp rpos sp1 sp2 
                        parse-data grind-fn dim-fn match)
  (let ((x))
    (if (or (and rbp (not (integerp (setq x rbp))))
            (and lbp (not (integerp (setq x lbp)))))
        (merror
          "syntax extension: binding powers must be integers; found: ~A"
          x))
    (if (stringp op) (setq op (define-symbol op)))
    (op-setup op)
    (let ((noun ($nounify op))
          (dissym (cdr (exploden op))))
      (cond
        ((not match)
         (setq dissym
               (append (if sp1 '(#\space)) dissym (if sp2 '(#\space)))))
        (t
         (if (stringp match) (setq match (define-symbol match)))
         (op-setup match)
         (putprop op match 'match)
         (putprop match 5 'lbp)
         (setq dissym (cons dissym (cdr (exploden match))))))
      (putprop op pos 'pos)
      (putprop op (cdr parse-data) (car parse-data))
      (putprop op grind-fn 'grind)
      (putprop op dim-fn 'dimension)
      (putprop noun dim-fn 'dimension)
      (putprop op dissym 'dissym)
      (putprop noun dissym 'dissym)
      (when rbp
        (putprop op rbp 'rbp)
        (putprop noun rbp 'rbp))
      (when lbp
        (putprop op lbp 'lbp)
        (putprop noun lbp 'lbp))
      (when lpos (putprop op lpos 'lpos))
      (when rpos (putprop op rpos 'rpos))
      (getopr op))))

(defun op-setup (op)
  (declare (special *mopl* $props))
  (let ((dummy (or (get op 'op) (coerce (makestring1 op) 'string))))
    (putprop op dummy 'op)
    (putopr dummy op)
    (if (and (operatorp1 op) (not (member dummy (cdr $props) :test #'eq)))
        (push dummy *mopl*))
    (add2lnc dummy $props)))

;(defun kill-operator (op)
;  (let
;    ((opr (getprop op 'op))
;     (noun-form ($nounify op)))
;    (unless (member opr *builtin-$props* :test #'equal)
;      (undefine-symbol opr)
;      (remopr opr)
;      (rempropchk opr)
;      (mapc #'(lambda (x) 
;                (remprop op x))
;            '(nud nud-expr nud-subr     ; NUD info
;              led led-expr led-subr     ; LED info
;              lbp rbp                   ; Binding power info
;              lpos rpos pos             ; Part-Of-Speech info
;              grind dimension dissym    ; Display info
;              op))                      ; Operator info
;      (mapc #'(lambda (x)
;                (remprop noun-form x)) 
;            '(dimension dissym lbp rbp)))))

;;; ----------------------------------------------------------------------------
