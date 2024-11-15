;;; built-in-methods.lisp --- Some Shoes built-in methods and functions

;; File:        built-in-methods.lisp
;; Description: Some Shoes built-in methods and functions
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-08 17:22
;; Version: 0.0.0
;; Last-Updated: 2024-11-11 00:51
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

(defun alert (message &key (title "Shoes says: ") (width 300) (height 200)
              &allow-other-keys)
  "Pops up a window containing a short message. "
  (clog-gui:alert-dialog (clog) message
                         :title  title
                         :width  width
                         :height height))

;; Note: I use a trival-channels to mimic the original sync ask
;; Maybe it would be better if using an async ask?

;; TODO:
;; make the ask support different input, like the Emacs's
;; interactive

(defun ask (message &key (title "Shoes asks: ") (width 300) (height 200)
                    (default-value "")
            &allow-other-keys)
  "Pops up a window and asks a question. "
  (let ((chn (trivial-channels:make-channel)))
    (clog-gui:input-dialog (clog) message
                           (lambda (content)
                             (trivial-channels:sendmsg chn (or content "")))
                           :title  title
                           :width  width
                           :height height
                           :default-value default-value)
    (trivial-channels:recvmsg chn)))

;;; TODO:

;; (defun ask-color (title)
;;   "Pops up a color picker window.

;; The program will wait for a color to be picked, then gives you
;; back a Color object. See the Color help for some ways you can
;; use this color."
;;   )

;; (defun ask-open-file ()
;;   "Pops up an \"Open file...\" window.

;; It's the standard window which shows all of your folders and
;; lets you select a file to open. Hands you back the name of
;; the file."
;;   )

;; (defun ask-save-file ()
;;   "Pops up a \"Save file...\" window, similiar to `ask-open-file'. "
;;   )

;; (defun ask-open-folder ()
;;   ""
;;   )

;;; built-in-methods.lisp ends here
