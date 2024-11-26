;;; elementary-math.lisp --- Elemetary methematics

;; File:        elementary-math.lisp
;; Description: Elemetary methematics
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-22 17:40
;; Version: 0.0.0
;; Last-Updated: 2024-11-22 17:40
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.fns)

(declaim (inline 2+ 2- 2* 2/))

(defun 2+ (number)
  "Return `number' + 2. "
  (+ 2 number))

(defun 2- (number)
  "Return `number' - 2. "
  (- number 2))

(defun 2* (number)
  "Return `number' * 2. "
  (* number 2))

(defun 2/ (number)
  "Return `number' / 2. "
  (/ number 2))

;;; elementary-math.lisp ends here
