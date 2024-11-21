;;; utils.lisp --- Utils to build macros

;; File:        utils.lisp
;; Description: Utils to build macros
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-15 21:56
;; Version: 0.0.0
;; Last-Updated: 2024-11-15 21:56
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.macros)

(defun split-list-plist?-by-literal-keyword (list-plist?)
  "Split `list-plist?' into list and plist.
Return values are list and plist.

Note: this will split by literal keyword
and won't check if rest is plist or not. "
  (loop for (var . rest) on list-plist? by #'cdr
        if (keywordp var)
          return (values vars (cons var rest))
        collect var into vars
        finally (return (values vars rest))))

;;; utils.lisp ends here
