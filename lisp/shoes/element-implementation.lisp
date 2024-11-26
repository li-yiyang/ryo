;;; element-implementation.lisp --- Implementations of elements

;; File:        element-implementation.lisp
;; Description: Implementations of elements
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-06 20:52
;; Version: 0.0.0
;; Last-Updated: 2024-11-26 21:11
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

;;; Check

(defmethod checked? ((check check))
  (clog:checkedp check))

(defmethod (setf checked?) (checked (check check))
  (setf (clog:checkedp check) (if checked t nil)))

;;; EditBox

(defmethod text ((edit-box edit-box))
  (clog:text-value edit-box))

(defmethod (setf text) (text (edit-box edit-box))
  (setf (clog:text-value edit-box) text))

;;; EditLine

(defmethod text ((edit-line edit-line))
  (clog:text-value edit-line))

;; TODO: escape multiple lines
(defmethod (setf text) (text (edit-line edit-line))
  (setf (clog:text-value edit-line) text))

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

;;; Progress

(defmethod fraction ((progress progress))
  (/ (parse-float:parse-float (clog:value progress)) 100.0))

(defmethod (setf fraction) (decimal (progress progress))
  (cond ((<= 0.0 decimal 1.0) (setf (clog:value progress) (truncate (* 100 decimal))))
        ((<= 0   decimal 100) (setf (clog:value progress) (truncate decimal)))
        (t (errorf "Bad value for progress: ~A. (should be 0.0~~1.0 or 0~~100). "
                   decimal))))

;;; Radio

(defmethod checked? ((radio radio))
  (clog:value radio))

(defmethod (setf checked?) (checked (radio radio))
  (setf (clog:value radio) (if checked t nil)))

;;; Timers

(defmethod initialize-instance :after ((timer timer-class) &key)
  (start timer))

(defmethod start :before ((timer timer-class))
  (with-slots (running-p) timer
    (setf running-p t)))

(defmethod start ((timer animation))
  (with-slots (function thread fps) timer
    (setf thread (bt:make-thread
                  (let ((sleep (float (/ 1 fps))))
                    (lambda () (loop while (and (funcall function)
                                                (slot-value timer 'running-p))
                                     do (sleep sleep))))
                  :name (fmt "~A thread" timer)))))

(defmethod start ((timer every-sec))
  (with-slots (function thread sec) timer
    (setf thread (bt:make-thread
                  (lambda () (loop while (and (funcall function)
                                              (slot-value timer 'running-p))
                                   do (sleep sec))
                    :name (fmt "~A thread" timer))))))

(defmethod start ((timer timer))
  (with-slots (function thread sec running-p) timer
    (setf thread (bt:make-thread
                  (lambda () (sleep sec) (funcall function))
                  :name (fmt "~A thread" timer)))
    (setf running-p nil)))

(defmethod stop ((timer timer-class))
  (with-slots (running-p) timer
    (setf running-p nil)))

(defmethod toggle ((timer timer-class))
  (with-slots (thread) timer
    (if (bt:thread-alive-p thread)
        (stop timer)
        (start timer))))

;;; element-implementation.lisp ends here
