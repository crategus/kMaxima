;;;; Created on 2011-05-15 22:26:13

(in-package :kmaxima-tests)

(define-test tyi-string-stream
  (with-input-from-string (stream "abc")
    (assert-eql #\a (tyi stream *parse-stream-eof*))
    (assert-eql #\b (tyi stream *parse-stream-eof*))
    (assert-eql #\c (tyi stream *parse-stream-eof*))
    (assert-eql *parse-stream-eof* (tyi stream *parse-stream-eof*))))

(define-test tyi-file-stream
  (with-open-file (stream "tests/rtest_parser.txt")
    (assert-eql #\a (tyi stream *parse-stream-eof*))
    (assert-eql #\b (tyi stream *parse-stream-eof*))
    (assert-eql #\c (tyi stream *parse-stream-eof*))
    (assert-eql *parse-stream-eof* (tyi stream *parse-stream-eof*))))

(define-test parse-tyi-string
  (with-input-from-string (stream "abc")
    (let ((*parse-stream* stream)
          (*parse-tyi* nil))
      (assert-eql #\a (parse-tyi))
      (assert-eql #\b (parse-tyi-peek))
      (assert-eql #\b (parse-tyi))
      (assert-eql #\c (parse-tyi))
      (assert-eql #\x (unparse-tyi #\x ))
      (assert-equal '(#\y . #\x) (unparse-tyi #\y ))
      (assert-equal '(#\z #\y . #\x) (unparse-tyi #\z ))
      (assert-eql #\z (parse-tyi-peek))
      (assert-eql #\z (parse-tyi))
      (assert-eql #\y (parse-tyi-peek))
      (assert-eql #\y (parse-tyi))
      (assert-eql #\x (parse-tyi-peek))
      (assert-eql #\x (parse-tyi))
      (assert-eql *parse-stream-eof* (parse-tyi)))))

(define-test parse-tyi-file
  (with-open-file (stream "tests/rtest_parser.txt")
    (let ((*parse-stream* stream)
          (*parse-tyi* nil))
      (assert-eql #\a (parse-tyi))
      (assert-eql #\b (parse-tyi-peek))
      (assert-eql #\b (parse-tyi))
      (assert-eql #\c (parse-tyi))
      (assert-eql *parse-stream-eof* (parse-tyi)))))

(define-test scan-one-token
  (with-input-from-string (stream "quit 123 1.23 1.2e3 1.2d3")
    (let ((*parse-stream* stream)
          (*scan-buffered-token* (list nil))
          (*parse-tyi* nil))
      (assert-equal '$quit (scan-one-token t nil))
      (assert-equal 123 (scan-one-token t nil))
      (assert-equal 1.23 (scan-one-token t nil))
      (assert-equal 1.2e3 (scan-one-token t nil))
      (assert-equal 1.2d3 (scan-one-token t nil)))))

(define-test peek-one-token
  (with-input-from-string (stream "100 200")
    (let ((*parse-stream* stream)
          (*scan-buffered-token* (list nil))
          (*parse-tyi* nil))
      (assert-equal 100 (peek-one-token t nil))
      (assert-equal   t (car *scan-buffered-token*))
      (assert-equal 100 (cdr *scan-buffered-token*))
      (scan-one-token)
      (assert-equal nil (car *scan-buffered-token*))
      (assert-equal 200 (peek-one-token t nil))
      (scan-one-token)
      ;; At this point the stream is empty.
      ;; In Maxima the following gives an error, because of a bug in
      ;; the function peek-one-token.
      (assert-equal nil  (peek-one-token t nil))
      (assert-equal 'EOF (peek-one-token t 'EOF)))))

(define-test mread
  (with-input-from-string (stream "1+2; 3*4; 5;")
    ;; We read three expressions from the stream with mread.
    (assert-equal (mread stream) '((displayinput) nil ((mplus) 1 2)))
    (assert-equal (mread stream) '((displayinput) nil ((mtimes) 3 4)))
    (assert-equal (mread stream) '((displayinput) nil 5))
    ;; At this point the stream is empty.
    ;; Check that we return with the eof-file value from mread.
    (assert-equal (mread stream) nil)
    (assert-equal (mread stream 'EOF) 'EOF)
    (assert-equal (mread stream '(nil)) '(nil))
    (assert-equal (mread stream nil) nil)))
