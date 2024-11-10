;;; element-creation.lisp --- Functions and Macros to create Elements

;; File:        element-creation.lisp
;; Description: Functions and Macros to create Elements
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-08 18:06
;; Version: 0.0.0
;; Last-Updated: 2024-11-10 16:24
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

;; Button

(defun %button (text &rest styles &key &allow-other-keys)
  "Create a `button'. "
  (declare (ignore styles))
  (with-wrap-as-shoes (button button (clog:create-button *slot*
                                                         :class "ryo-shoes-button"
                                                         :content text))))

(defmacro button (text &body body)
  "Create a `button'. "
  (let ((button (gensym "BUTTON")))
    `(with-wrap (,button (%button ,text))
       ,(unless (endp body) `(click ,button ,@body)))))

;; Check

(defun %check (&rest styles &key &allow-other-keys)
  "Create a check box. "
  (declare (ignore styles))
  (with-wrap-as-shoes (check check (clog:create-form-element
                                    *slot* "checkbox"
                                    :class "ryo-shoes-check"))))

(defmacro check (&rest styles)
  "Create a check box. "
  `(%check ,@styles))

;; ListBox

(defun %list-box (list-box-items &key &allow-other-keys)
  "Create a `list-box'. "
  (with-wrap-as-shoes (list-box list-box
                        (clog:create-select *slot*
                                            :class "ryo-shoes-select"))
    (with-slots (items) list-box
      (loop for item in list-box-items do
        (progn
          (clog:add-select-option list-box item item)
          (push item items)))
      (setf items (nreverse items)))))

(defmacro list-box ((items &rest styles &key &allow-other-keys) &body body)
  "Create a `list-box'. "
  (if (endp body)
      `(%list-box ,items ,@styles)
      (let ((list-box (gensym "LIST-BOX")))
        `(with-wrap (,list-box (%list-box ,items ,@styles))
           (change ,list-box ,@body)))))

;; TimerClass

(defmacro animation (fps &body body)
  "Create an `animation' element with `fps'. "
  `(make-instance 'animation
                  :fps ,fps
                  :function (shoes-lambda () ,@body)))

(defmacro every-sec (sec &body body)
  "Create an `every-sec' element with `sec'. "
  `(make-instance 'every-sec
                  :sec      ,sec
                  :function (shoes-lambda () ,@body)))

(defmacro timer (sec &body body)
  "Create an `timer' element with `sec'. "
  `(make-instance 'timer
                  :sec ,sec
                  :function (shoes-lambda () ,@body)))

;;; element-creation.lisp ends here
