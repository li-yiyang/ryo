;;; type-convert.lisp --- Convert between types.

;; File:        type-convert.lisp
;; Description: Convert between types.
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-15 17:49
;; Version: 0.0.0
;; Last-Updated: 2024-11-15 17:49
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.macros)

(defmacro str-int (int-like-str &optional (base 10))
  "Convert `int-like-str' to `int' value. "
  `(parse-integer ,int-like-str :radix ,base))

(defmacro str-float (float-like-str &optional (base 10))
  "Convert `float-like-str' to `float' value. "
  `(parse-float:parse-float ,float-like-str :radix ,base))

;;; type-convert.lisp ends here
