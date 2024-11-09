;;; iter.lisp --- Macros for iteration

;; File:        iter.lisp
;; Description: Macros for iteration
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-10-31 10:58
;; Version: 0.0.0
;; Last-Updated: 2024-10-31 10:58
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.macros)

(defmacro iter-i* (bindings &body body)
  "Do iteration by bindings.

Syntax:
The `bindings' each element should be like:
    (var end)              ; 
    (var start end)
    (var start end step)
    (var start end step :reject expr more-keys)

"

;;; iter.lisp ends here
