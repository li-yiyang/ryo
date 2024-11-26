;;; config.lisp --- Configuration for RYO.SHOES

;; File:        config.lisp
;; Description: Configuration for RYO.SHOES
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-02 22:47
;; Version: 0.0.0
;; Last-Updated: 2024-11-10 15:56
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

;;; Configures for external packages
;; overwrittern the CLOG-C3 config to use the local JS file

(setf clog-c3:*clog-c3-js-path*  "/js/c3.min.js")
(setf clog-c3:*clog-d3-js-path*  "/js/d3.v5.min.js")
(setf clog-c3:*clog-c3-css-path* "/css/c3.css")

;;; Config for Shoes Server

(defparameter *shoes-port* 2333
  "Default server port for RYO.SHOES to run.

Note: this will also be changed when calling `boot-shoes'
with parameter `:port', the port value will be bind to
`*shoes-port*'. ")

(defparameter *shoes-title* "RYO.SHOES"
  "Default application title name. ")

;;; Shoes Closure Variables

(defparameter *clog* nil
  "Current CLOG connection. ")

(defparameter *app* nil
  "Current Shoes app window. ")

(defparameter *slot* nil
  "Current Slot.

Develop Note:
It's like opening the box and enter to it. ")

(defparameter *self* nil
  "Current handler function caller.

Example:

    (button \"Click Me\"
      (fmt! \"*self* is button ~A\" *self*))

Develop Note:
`*self* should only change when defining a `shoes-lambda',
mostly frequently used in Events. ")

;;; config.lisp ends here
