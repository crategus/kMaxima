;;;; Created on 2011-05-15 22:26:13

(load "lisp/lisp-unit.lisp")

(defpackage :kmaxima-tests
  (:use :common-lisp :lisp-unit :kmaxima))

(in-package :kmaxima-tests)

(define-test tyi-string-stream
  (with-input-from-string (stream "abc")
    (assert-equal #\a (tyi stream *parse-stream-eof*))
    (assert-equal #\b (tyi stream *parse-stream-eof*))
    (assert-equal #\c (tyi stream *parse-stream-eof*))
    (assert-equal *parse-stream-eof* (tyi stream *parse-stream-eof*))
))

(define-test tyi-file-stream
  (with-open-file (stream "tests/rtest_parser.mac")
    (assert-equal #\a (tyi stream *parse-stream-eof*))
    (assert-equal #\b (tyi stream *parse-stream-eof*))
    (assert-equal #\c (tyi stream *parse-stream-eof*))
    (assert-equal *parse-stream-eof* (tyi stream *parse-stream-eof*))
))

(define-test parse-tyi-string
  (with-input-from-string (stream "abc")
    (setq *parse-stream* stream)
    (assert-equal #\a (parse-tyi))
    (assert-equal #\b (parse-tyipeek))
    (assert-equal #\b (parse-tyi))
    (assert-equal #\c (parse-tyi))
    (assert-equal *parse-stream-eof* (parse-tyi))))

(define-test parse-tyi-file
  (with-open-file (stream "tests/rtest_parser.mac")
    (setq *parse-stream* stream)
    (assert-equal #\a (parse-tyi))
    (assert-equal #\b (parse-tyipeek))
    (assert-equal #\b (parse-tyi))
    (assert-equal #\c (parse-tyi))
    (assert-equal *parse-stream-eof* (parse-tyi))
    (close stream)))

(define-test scan-one-token
  (with-input-from-string (stream "quit 123 1.23 1.2e3 1.2d3")
    (setq *parse-stream* stream)
    (assert-equal '$quit (scan-one-token t nil))
    (assert-equal 123 (scan-one-token t nil))
    (assert-equal 1.23 (scan-one-token t nil))
    (assert-equal 1.2e3 (scan-one-token t nil))
    (assert-equal 1.2d3 (scan-one-token t nil))
))
