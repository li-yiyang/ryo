;;; ryo.asd --- ASDF system definition for RYO

;; File:        ryo.asd
;; Description: ASDF system definition for RYO
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-10-31 01:15
;; Version: 0.0.0
;; Last-Updated: 2024-11-10 22:27
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(asdf:defsystem #:ryo
  :author ("凉凉")
  :version "0"
  :description
  "This is a package contains some easy to use scripts"
  :long-description
  ""
  :depends-on (trivial-indent
               trivial-garbage
               trivial-channels
               str
               clog clog-c3
               parse-float
               qbase64
               lparallel)
  :serial t
  :components
  (
   ;; some C codes, submodules should goes to here
   ;; yes, i mean that you can definitely enjoy
   ;; using all those C stuffs with Lisp wrapper
   ;;
   ;; the C programs should be built into shared
   ;; dynamic library for Lisp to call with CFFI
   ;; those library should be generated via Makefile
   ;; and installed under ryo/lib
   ;;
   ;; how ever, if some could not fit into dynamic
   ;; library or I have to call it using `uiop:run-program'
   ;; or `uiop:launch-program', these executable
   ;; should be installed under ryo/bin
   (:module c
    :pathname "c")

   (:module lib
    :pathname "lib")

   (:module bin
    :pathname "bin")

   ;; some static files goes under ryo/statics
   ;; as for CLOG hosting static root
   (:module   statics
    :pathname "statics"
    :components
    ((:module js
      :pathname "js"
      :components
      ())

     (:module css
      :pathname "css"
      :components
      ())))

   ;; the lisp module contains the Lisp code for RYO
   (:module lisp
    :pathname "lisp"
    :components
    (
     ;; each package should define its exported symbols
     ;; in package.lisp and implement their own code
     ;; under each submodule
     (:file "package")

     (:module ryo.macros
      :pathname "macros"
      :components
      ((:file "format")
       (:file "condition")
       (:file "restart-handler")
       ))

     (:module ryo.shoes
      :pathname "shoes"
      :components
      (;;; Config
       ;; configure for Shoes
       ;; declaration for some closure variables
       (:file "config")
       (:file "utils")
       (:file "image-utils")
       (:file "text-like-utils")

       ;;; some low-level CLOG bindings
       (:file "shoes-server")

       ;;; Top-level Documentation
       (:file "documentation")
       (:file "common-implementation")

       ;;; Slots
       (:file "slot-creation")

       ;;; Elements
       (:file "element-creation")

       ;; TextLike
       (:file "text-like-implementation")

       ;; app
       (:file "app-creation")
       (:file "app-implementation")
       (:file "built-in-methods")

       ))))))

;;; ryo.asd ends here
