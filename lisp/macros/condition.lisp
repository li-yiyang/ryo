;;; condition.lisp --- Macros to simplify list manipulation

;; File:        condition.lisp
;; Description: Macros to simplify list manipulation
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-10-31 10:37
;; Version: 0.0.0
;; Last-Updated: 2024-10-31 10:37
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.macros)

(defmacro length= (sequence len)
  "Test if `sequence' length is equal to `len'. "
  `(= (length ,sequence) ,len))

(defmacro length/= (sequence len)
  "Test if `sequence' length is not equal to `len'. "
  `(/= (length ,sequence) ,len))

(defmacro length> (sequence len)
  "Test if `sequence' length is less than `len'. "
  `(< (length ,sequence) ,len))

(defmacro length< (sequence len)
  "Test if `sequence' length is greater than `len'. "
  `(> (length ,sequence) ,len))

(defmacro length>= (sequence len)
  "Test if `sequence' length is less or equal than `len'. "
  `(<= (length ,sequence) ,len))

(defmacro length<= (sequence len)
  "Test if `sequence' length is greater or equal than `len'. "
  `(>= (length ,sequence) ,len))

(defmacro neq (a b)
  "Test if `a' and `b' are not `eq'. "
  `(not (eq ,a ,b)))

;;; condition.lisp ends here
