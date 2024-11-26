;;; format.lisp --- Macros to simplify format.

;; File:        format.lisp
;; Description: Macros to simplify format.
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-10-31 10:19
;; Version: 0.0.0
;; Last-Updated: 2024-11-23 16:12
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.macros)

(defmacro errorf (message &rest format-arguments)
  "Raise error with message.

Example:
  (errorf \"simple error\") ;; => (error \"simple error\")
  (errorf \"~Dth error\" i) ;; => (error (format nil \"~Dth error\" i))

Para:
 + `message': string as format control string
 + `format-arguments': if provided, will used as `format' arguments

See: `ryo.macros:warnf'"
  (let ((message (if (endp format-arguments)
                     message
                     `(format nil ,message ,@format-arguments))))
    `(error ,message)))

(defmacro warnf (message &rest format-arguments)
  "Warn with message.

Example:
  (warnf \"simple error\") ;; => (warn \"simple error\")
  (warnf \"~Dth error\" i) ;; => (warn (format nil \"~Dth error\" i))

Para:
 + `message': string as format control string
 + `format-arguments': if provided, will used as `format' arguments

See: `ryo.macros:errorf'"
  (let ((message (if (endp format-arguments)
                     message
                     `(format nil ,message ,@format-arguments))))
    `(warn ,message)))

(defmacro fmt (control-string &rest args)
  "Format as a string, shortcut for (format nil ...) .

Example:
  (fmt \"Hello ~A~%\" 'me)
"
  `(format nil ,control-string ,@args))

(defmacro fmt! (control-string &rest args)
  "Format to `*standard-output*', shotcut for `format'.
See `fmt' for formatting to string. "
  `(format *standard-output* ,control-string ,@args))

(defmacro with-output-to-strings
    ((stream* &rest more-streams*) progn &body body)
  "Like `with-output-to-string' but supports multiple streams.
Return values of strings for each stream.

Syntax:
 * the `stream*' should be like single symbol or (string stream)

Example:
  (with-output-to-strings (stream1 (string2 stream2))
    (progn
      (format stream1 \"to stream 1\")
      (format stream2 \"to stream 2\"))
    (values stream1 string2))
"
  (let ((stream* (mapcar (lambda (stream*)
                           (if (listp stream*)
                               stream*
                               (list stream* stream*)))
                         (cons stream* more-streams*))))
    `(let (,@(mapcar #'first stream*))
       ,(loop for (str strm)  in stream*
              for prog = `(setf ,str (with-output-to-string (,strm) ,progn))
                then `(setf ,str (with-output-to-string (,strm) ,prog))
              finally (return prog))
       ,@(if (endp body)
             `((values ,@(mapcar #'first stream*)))
             body))))

;; for SLIME/SLY
(trivial-indent:define-indentation with-output-to-strings (4 2 &body))

;;; format.lisp ends here
