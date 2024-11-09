;;; app-implementation.lisp --- Implementation for App methods

;; File:        app-implementation.lisp
;; Description: Implementation for App methods
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-08 17:37
;; Version: 0.0.0
;; Last-Updated: 2024-11-08 17:37
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

(defmethod app-local-binding ((app app) binding-sym)
  (with-slots (bindings) app
    (or (gethash binding-sym bindings nil)
        (warnf "Missing bingdings of `~A' within App ~A" binding-sym app))))

(defmethod (setf app-local-binding) (binding (app app) binding-sym)
  (with-slots (bindings) app
    (setf (gethash binding-sym bindings) binding)))

(defmacro @ (name &optional (expr nil expr?))
  "Set `name' binded with `expr' result (with given `expr'),
or just Get app local binding of `name' (without given `expr'). "
  (if expr?
      `(setf (app-local-binding *app* ',name) ,expr)
      `(app-local-binding *app* ',name)))

;;; app-implementation.lisp ends here
