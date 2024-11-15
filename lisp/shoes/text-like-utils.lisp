;;; text-like-utils.lisp --- Utils for text-like

;; File:        text-like-utils.lisp
;; Description: Utils for text-like
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-12 12:41
;; Version: 0.0.0
;; Last-Updated: 2024-11-12 12:41
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

;; This file is to make the `text-like' more easier to use and define
;;
;; the idea is like below:
;;
;; (para "a" "b" "c" ...
;;       :key ... :key ...)

(defun parse-text-block-body (body*)
  "Parse the `text-block' body and return `body' and `styles-plist'. "
  (loop for (expr . rest) on body* by #'cdr
        while (not (keywordp expr))
        collect expr into body
        finally (return (values body
                                (when (keywordp expr)
                                  (cons expr rest))))))

(defun expand-define-text-block-texts* (texts*)
  "Trun `define-text-block' into something like a body definition. "
  (multiple-value-bind (texts styles)
      (parse-text-block-body texts*)
    (let ((body ()))
      (push `(%text-block ,(first texts)) body)
      (loop with join = (getf styles :join :none)
            for text in (rest texts)
            do (ecase join
                 ((:newline :br) (push `(clog:create-br *slot*) body))
                 (:none t))
            do (push `(%text-block ,text) body)
            finally (return (nreverse body))))))

;;; text-like-utils.lisp ends here
