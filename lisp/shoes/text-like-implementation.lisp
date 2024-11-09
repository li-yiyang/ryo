;;; text-like-implementation.lisp --- Implementation for Text-like

;; File:        text-like-implementation.lisp
;; Description: Implementation for Text-like
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-09 01:03
;; Version: 0.0.0
;; Last-Updated: 2024-11-09 01:03
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

(defmethod %text-block (obj)
  (span (text obj)))

(defmethod %text-block ((text text-like))
  text)

(defmethod text (obj)
  (fmt "~A" obj))

(defmethod text ((text text-like))
  (clog:text text))

(defmethod (setf text) ((string string) (text text-like))
  (setf (clog:text text) string))

;;; text-like-implementation.lisp ends here
