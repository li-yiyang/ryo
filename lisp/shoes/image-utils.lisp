;;; image-utils.lisp --- Utils to processing the image.

;; File:        image-utils.lisp
;; Description: Utils to processing the image.
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-10 18:25
;; Version: 0.0.0
;; Last-Updated: 2024-11-10 22:18
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

;; TODO:
;; make this more better and efficient
(defun load-image-to-base64 (path)
  "Load image and return Base64 file. "
  (with-open-file (image path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length image) :element-type '(unsigned-byte 8))))
      (read-sequence buffer image)
      (fmt "data:image/~A;base64,~A"
           (pathname-type path)
           (qbase64:encode-bytes buffer)))))

;;; image-utils.lisp ends here
