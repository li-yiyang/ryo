;;; slot-creation.lisp --- Create Slots

;; File:        slot-creation.lisp
;; Description: Create Slots
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-08 16:01
;; Version: 0.0.0
;; Last-Updated: 2024-11-08 16:01
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

(defun %stack (block &rest styles &key width height &allow-other-keys)
  "Create a new Stack. "
  (declare (ignorable styles))
  (with-wrap-as-shoes
      (*slot* stack (clog:create-div *slot* :class "ryo-shoes-stack"))
    (when width  (setf (width  *slot*) width))
    (when height (setf (height *slot*) height))
    (funcall block)))

(defmacro stack ((&rest styles &key width height &allow-other-keys) &body body)
  "Create a new Stack. "
  (declare (ignore width height))
  `(%stack (lambda () ,@body) ,@styles))

(defun %flow (block &rest styles &key width height &allow-other-keys)
  "Create a new Flow. "
  (declare (ignorable styles))
  (with-wrap-as-shoes
      (*slot* flow (clog:create-div *slot* :class "ryo-shoes-stack"))
    (when width  (setf (width  *slot*) width))
    (when height (setf (height *slot*) height))
    (funcall block)))

(defmacro flow ((&rest styles &key width height &allow-other-keys) &body body)
  "Create a new Stack. "
  (declare (ignore width height))
  `(%flow (lambda () ,@body) ,@styles))

;;; slot-creation.lisp ends here
