;;; format.lisp --- Macros to simplify format.

;; File:        format.lisp
;; Description: Macros to simplify format.
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-10-31 10:19
;; Version: 0.0.0
;; Last-Updated: 2024-11-06 16:41
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

;;; format.lisp ends here
