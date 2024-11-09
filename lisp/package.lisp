;;; package.lisp --- Package definition for RYO

;; File:        package.lisp
;; Description: Package definition for RYO
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-10-31 10:11
;; Version: 0.0.0
;; Last-Updated: 2024-11-09 14:12
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(defpackage #:ryo.macros
  (:use :cl)
  (:documentation
   "RYO.MACROS is a set of handly macros")
  (:export
   ;; format
   #:errorf
   #:warnf
   #:fmt
   #:fmt!

   ;; condition
   #:length=
   #:length/=
   #:length>
   #:length<
   #:length<=
   #:length>=
   #:neq

   ;; restart-handler
   #:assert-restart))

(defpackage #:ryo.shoes
  (:use :cl :ryo.macros)
  (:documentation
   "RYO.SHOES is used for making GUI based on CLOG")
  (:export
   ;; config
   #:*shoes-port*
   #:*shoes-title*

   ;; closures
   #:*app*
   #:*slot*

   ;; Slots
   #:stack
   #:flow

   ;;; Elements
   #:button

   ;;; TextLike
   ;; TextBlock
   #:banner
   #:title
   #:subtitle
   #:tagline
   #:caption
   #:para
   #:inscription
   ;; TextClass
   #:em
   #:span
   #:ins
   #:link
   #:strong

   ;;; Manipulation
   #:parent
   #:contents
   #:width
   #:height
   #:hide
   #:show
   #:toggle
   #:text
   #:owner
   #:@

   ;; shoes server
   #:shutdown-shoes
   #:boot-shoes
   #:reboot-shoes
   ))

(defpackage #:ryo
  (:use :cl :ryo.macros))

;;; package.lisp ends here
