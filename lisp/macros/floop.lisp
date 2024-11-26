;;; floop.lisp --- Loop on File (Syntax like AWK, aimed to acclerate File reading program)

;; File:        floop.lisp
;; Description: Loop on File (Syntax like AWK, aimed to acclerate File reading program)
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-15 16:35
;; Version: 0.0.0
;; Last-Updated: 2024-11-15 16:35
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.macros)

(defparameter *nr*   0  "Nth read count")
(defparameter *line* () "Element of each line fields. ")
(defparameter *nf*   0  "Number of fields of *line*. ")

(defmacro with-read-file-or-stream ((stream path-or-stream) &body body)
  "With open `path-or-stream' for reading.
The `stream' will be used as the opened stream. "
  (alexandria:with-gensyms (p-or-s abort-p)
    `(let* ((,p-or-s  ,path-or-stream)
            (,stream  (if (streamp ,p-or-s) ,p-or-s (open ,p-or-s)))
            (,abort-p t))
       (unwind-protect (progn ,@body (setf ,abort-p t))
         (when ,stream (close ,stream :abort ,abort-p))))))

;; TODO:
;; make the *NF*, *LINE*, *NF* lazy calculated
;; may not need to read the whole file

(defmacro floop ((path-or-stream &key (seperator " ")) &body pattern-action-pairs)
  "Iterate loop on `path-or-stream'.

The

Syntax:
The `pattern-action-pairs' will share a AWK like syntax:
for each pattern action pair, it should be like

    (pattern . action)

If the pattern is `t' or `:every', then the action will
be executed.

The `path-or-stream' will be redirected into `*standard-input*'.

Use:
+ *NR*:   readed line number
+ *LINE*: element of each line fields
+ *NF*:   number of field elements
"
  (alexandria:with-gensyms (raw)
    `(with-read-file-or-stream (*standard-input* ,path-or-stream)
       (loop for ,raw = (read-line *standard-input* nil nil)
             while ,raw
             for *nr* from 1
             for *line* = (cl-ppcre:split ,seperator ,raw)
             for *nf*   = (length *line*)
             ,@(apply #'nconc
                      (mapcar (lambda (pap) `(if ,(car pap) do (progn ,@(cdr pap))))
                              pattern-action-pairs))))))

;;; floop.lisp ends here
