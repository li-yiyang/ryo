;;; element-creation.lisp --- Functions and Macros to create Elements

;; File:        element-creation.lisp
;; Description: Functions and Macros to create Elements
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-08 18:06
;; Version: 0.0.0
;; Last-Updated: 2024-11-08 18:06
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

(defmacro button (text &body body)
  "Create a Button. "
  (let ((button (gensym "BUTTON")))
    `(with-wrap-as-shoes (,button button (clog:create-button *slot*
                                                            :class "ryo-shoes-button"
                                                            :content ,text))
       ,(unless (endp body)
          `(click (,button) ,@body)))))

;;; element-creation.lisp ends here