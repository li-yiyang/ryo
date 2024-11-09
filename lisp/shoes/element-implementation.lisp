;;; element-implementation.lisp --- Implementations of elements

;; File:        element-implementation.lisp
;; Description: Implementations of elements
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-06 20:52
;; Version: 0.0.0
;; Last-Updated: 2024-11-10 01:14
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

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
