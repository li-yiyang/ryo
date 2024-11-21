;;; setf.lisp --- Macros helping with setf

;; File:        setf.lisp
;; Description: Macros helping with setf
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-16 00:52
;; Version: 0.0.0
;; Last-Updated: 2024-11-16 00:58
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.macros)

(defmacro cyclef (var1 var2 &rest more-vars)
  "Set and reset values.

This is much like the shiftf, the value of expressions will
be passed from right to left and the first variable's value
will be passed to the last variable.

Example:

    (cyclef a b c)

will set a = b, b = c, c = a. "
  `(setf ,(if (endp more-vars) var2 (car (last more-vars)))
         (shiftf ,var1 ,var2 ,@more-vars)))

(defmacro swapf (a b)
  "Swap two var's value. "
  `(cyclef ,a ,b))

;;; setf.lisp ends here
