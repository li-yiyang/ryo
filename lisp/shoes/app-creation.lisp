;;; app-creation.lisp --- Create a Shoes App

;; File:        app-creation.lisp
;; Description: Create a Shoes App
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-06 17:55
;; Version: 0.0.0
;; Last-Updated: 2024-11-09 14:36
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

(defun %window (block &rest styles &key (owner *app*) (title *shoes-title*)
                      (width 400) (height 400) (resizable t) (centerp t)
                      &allow-other-keys)
  "Create a new App window. "
  (declare (ignore styles))
  (let* ((*slot* nil)
         (*app*  (clog-gui:create-gui-window (clog)
                                             :title  title
                                             :width  width
                                             :height height
                                             :hidden centerp)))
    (change-class *app* 'app :owner owner)
    (when centerp
      (clog-gui:window-center *app*)
      (setf (clog:visiblep *app*) t))
    (unless resizable
      (clog-gui:set-on-window-can-size *app* (lambda (app) (declare (ignore app)) nil)))
    (with-wrap-as-shoes (*slot* stack (clog-gui:window-content *app*))
      ;; terminate the app and throw errors if error
      (handler-case (funcall block)
        (t (err) (clog-gui:window-close *app*) (error err))))
    *app*))

(defmacro window ((&rest styles &key owner title width height resizable centerp &allow-other-keys)
                  &body body)
  "Open a new App Window. "
  (declare (ignore owner title width height resizable centerp))
  `(%window (lambda () ,@body) ,@styles))

;;; app-creation.lisp ends here
