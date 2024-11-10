;;; element-implementation.lisp --- Implementations of elements

;; File:        element-implementation.lisp
;; Description: Implementations of elements
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-06 20:52
;; Version: 0.0.0
;; Last-Updated: 2024-11-10 15:56
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

;;; Button

(defmethod text ((button button))
  (clog:text button))

(defmethod (setf text) ((text string) (button button))
  (setf (clog:text button) text))

;;; ListBox

(defmethod text ((list-box list-box))
  (clog:select-text list-box))

(defmethod choose ((list-box list-box) item)
  (setf (clog:value list-box) item)
  (clog:jquery-execute list-box "trigger(\"change\")"))

(defmethod items ((list-box list-box))
  (slot-value list-box 'items))

(defmethod (setf items) ((items list) (list-box list-box))
  (setf (clog:inner-html list-box) "")
  (loop for item in items do
    (clog:add-select-option list-box item item))
  (setf (slot-value list-box 'items) items))

;;; Timers

(defmethod initialize-instance :after ((timer timer-class) &key)
  (start timer))

(defmethod start ((timer animation))
  (with-slots (function thread fps) timer
    (setf thread (bt:make-thread
                  (let ((sleep (float (/ 1 fps))))
                    (lambda () (loop do (sleep sleep) do (funcall function))))
                  :name (fmt "~A thread" timer)))))

(defmethod start ((timer every-sec))
  (with-slots (function thread sec) timer
    (setf thread (bt:make-thread
                  (lambda () (loop do (sleep sec) do (funcall function)))
                  :name (fmt "~A thread" timer)))))

(defmethod start ((timer timer))
  (with-slots (function thread sec) timer
    (setf thread (bt:make-thread
                  (lambda () (sleep sec) (funcall function))
                  :name (fmt "~A thread" timer)))))

(defmethod stop ((timer timer-class))
  (with-slots (thread) timer
    (bt:destroy-thread thread)))

(defmethod toggle ((timer timer-class))
  (with-slots (thread) timer
    (if (bt:thread-alive-p thread)
        (stop timer)
        (start timer))))

;;; element-implementation.lisp ends here
