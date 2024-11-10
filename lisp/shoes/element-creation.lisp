;;; element-creation.lisp --- Functions and Macros to create Elements

;; File:        element-creation.lisp
;; Description: Functions and Macros to create Elements
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-08 18:06
;; Version: 0.0.0
;; Last-Updated: 2024-11-10 17:45
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

(defshoes-element check nil (styles)
  "Create a check box. "
  (with-wrap-as-shoes (check check (clog:create-form-element
                                    *slot* "checkbox"
                                    :class "ryo-shoes-check"))))

;; EditBox

(defshoes-element edit-box change (styles width (height 100))
  "Create a `edit-box'. "
  (with-wrap-as-shoes (edit edit-box (clog:create-text-area
                                      *slot*
                                      :class "ryo-shoes-edit-box"))
    (when width  (setf (clog:width  edit) width))
    (when height (setf (clog:height edit) height))))

;; EditLine

(defshoes-element edit-line change (styles width passwd-p)
  "Create a `edit-line'. "
  (with-wrap-as-shoes (edit edit-line (clog:create-form-element
                                       *slot* (if passwd-p "password" "text")
                                       :class "ryo-shoes-edit-line"))
    (when width (setf (clog:width edit) width))))

;; Image

(defun image (path &rest styles &key width height &allow-other-keys)
  "Create an image. "
  (cond ((stringp path)
         (with-wrap-as-shoes (image image (clog:create-img *slot* :url-src path))
           (when width  (setf (clog:width  image) width))
           (when height (setf (clog:height image) height))))
        ((pathnamep path)
         (apply #'image (load-image-to-base64 path) styles))
        (t (errorf "To load image from ~A is not implemented yet" path))))

;; ListBox

(defshoes-element list-box change (styles items)
  "Create a `list-box'. "
  (with-wrap-as-shoes (list-box list-box
                        (clog:create-select *slot*
                                            :class "ryo-shoes-select"))
    (with-slots ((list-box-items items)) list-box
      (loop for item in items do
        (progn
          (clog:add-select-option list-box item item)
          (push item list-box-items)))
      (setf list-box-items (nreverse list-box-items)))))

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
