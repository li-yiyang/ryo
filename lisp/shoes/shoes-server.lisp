;;; shoes-server.lisp --- Start the CLOG server for RYO.SHOES

;; File:        shoes-server.lisp
;; Description: Start the CLOG server for RYO.SHOES
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-08 14:24
;; Version: 0.0.0
;; Last-Updated: 2024-11-08 14:24
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

(defun clog ()
  "Get `*clog*' CLOG body. "
  (assert-restart (clog:is-running-p)
    :report "Start RYO.Shoes server. "
    (reboot-shoes))
  (assert-restart (clog:validp *clog*)
    :report "Create new browser window to RYO.Shoes. "
    (clog:open-browser :url (fmt "http://localhost:~A" *shoes-port*)))
  *clog*)

;; TODO: make Shoes Desktop more powerful to use
(defun on-new-shoes (body)
  "Render new Shoes desktop. "
  (clog-gui:clog-gui-initialize body)
  (clog:load-css (clog:html-document body) "/css/ryo-shoes.css")
  (setf *clog* body)
  (let* ((menu-bar (clog-gui:create-gui-menu-bar  body))
         (icon     (clog-gui:create-gui-menu-icon menu-bar :image-url "/img/icon.png")))
    (declare (ignore icon apps))))

(defun boot-shoes (&key (port *shoes-port*))
  "Start RYO.SHOES server.

Para:
 + `port': the server port (by default to be `*shoes-port*')
   setting this will also change the `*shoes-port*' value
"
  (setf *shoes-port* port)
  (clog:initialize 'on-new-shoes
                   :port port
                   :static-root (asdf:system-relative-pathname :ryo "./statics/")))

(defun shutdown-shoes ()
  "Shutdown RYO.SHOES server. "
  (clog:shutdown))

(defun reboot-shoes (&key (port *shoes-port*))
  "Shutdown and reboot RYO.SHOES server.

Para:
 * `port': if changing the server port
"
  (when (clog:is-running-p) (clog:shutdown))
  (boot-shoes :port port))

;;; shoes-server.lisp ends here
