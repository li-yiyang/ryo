;;; restart-handler.lisp --- Macros about restart and handler

;; File:        restart-handler.lisp
;; Description: Macros about restart and handler
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-06 18:19
;; Version: 0.0.0
;; Last-Updated: 2024-11-06 18:19
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.macros)

(defmacro assert-restart (test-form &body body)
  "Restart with `error-fmt' poped out if `test-form' fails. "
  `(restart-case (assert ,test-form)
     (try ()
       ,@body)))

;;; restart-handler.lisp ends here
