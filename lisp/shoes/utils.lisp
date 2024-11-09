;;; utils.lisp --- Some utils functions

;; File:        utils.lisp
;; Description: Some utils functions
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-08 16:05
;; Version: 0.0.0
;; Last-Updated: 2024-11-09 14:04
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

(defmacro with-wrap-as-shoes ((var shoes-class &body exprs) &body body)
  "To wrap the `exprs' result as `shoes-class' and do some process in `body'.
This will add `var' to Slot contents if `*slot*' is none-nil. "
  (let ((tmp (gensym (fmt "~@:(~A~)" var))))
    `(let ((,tmp (progn ,@exprs)))
       (change-class ,tmp ',shoes-class)
       ;; add to parents
       (when *slot* (push ,tmp (slot-value *slot* 'contents)))
       (let ((,var ,tmp))
         ,@body
         ,var))))

(defmacro with-div-wrap-as-shoes ((var shoes-class &optional css-class) &body body)
  "To wrap the `clog:clog-div' with css-class as shoes class. "
  `(with-wrap-as-shoes (,var ,shoes-class (clog:create-div *slot* :class ,css-class))
     ,@body))

(defmacro with-span-wrap-as-shoes ((var shoes-class &optional css-class) &body body)
  "To wrap the `clog:clog-span' with `css-class' as shoes class.
The span content is the result of `body'. "
  `(with-wrap-as-shoes (,var
                        ,shoes-class
                         (clog:create-span *slot*
                                           :class ,css-class
                                           :content (fmt "~A" (progn ,@body))))))

;; TODO:
;; is there any better way to make this more wiser?
(defmacro shoes-lambda (lambda-list &body body)
  "Tricky patches for the original lambda.

This returns a lambda function with closure variables: `*slot*', `*app*',
which means the lambda function will be able to run safely outside
the original closure environment.

This is little tricky and should only be used when dealing with
events, timer (threads) code rather than directly exposed to
normal user. "
  (let ((slot (gensym "SLOT"))
        (app  (gensym "APP")))
    (loop for (expr . rest) on body by #'cdr
          while (and (listp expr) (eq (first expr) 'declare))
          collect expr into declarations
          finally (return
                    `(let ((,slot *slot*)
                           (,app  *app*))
                       (lambda ,lambda-list
                         ,@declarations
                         (let ((*app*  ,app)
                               (*slot* ,slot))
                           (declare (ignorable *app* *slot*))
                           ,@(cons expr rest))))))))

;;; utils.lisp ends here
