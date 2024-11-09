;;; ryo.asd --- ASDF system definition for RYO

;; File:        ryo.asd
;; Description: ASDF system definition for RYO
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-10-31 01:15
;; Version: 0.0.0
;; Last-Updated: 2024-11-09 14:27
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
               str
               clog clog-c3
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

       ))

     (:module ryo.shoes
      :pathname "shoes"
      :documentation
      "Shoes is inspired by _why's Shoes (Ruby Shoes 3), but is not Shoes.

`RYO.SHOES' is aimed to create a Shoes-like DSL layer on `CLOG'
package, together with some JS and CSS scripts to make GUI more
easier to write.

The `RYO.SHOES' is constructed following The Shoes Manual,
which should be able to found at shoesrb.com.

==============================================================
The Rules of Shoes
`RYO.SHOES' use closure variable to store the current environment.
Whenever you create a new window, `*app*' is bind to the window.

    (window (:title \"MAIN\")
      (para (fmt \"~A\" *app*))
      (button \"Spawn\"
        (window (:title \"CHILD\")
          (para *app*))))

Note: before you can create any window, the shoes server should
running in the backend `boot-shoes' and at least one connection
to the Shoes app (if not, will open a browser visiting it).

Shoes use a concept of `*slot*' as the abstract of current
panel box, like the <div> in HTML. Two types of slots here:
+ `stack' is the <div> displayed as block
+ `flow' is the <div> displayed as inline-block

Note: this \"slot\" is different from CLOS's variable slots.

By default, all the drawings goes to the `*slot*', using method
`appending-by' or macro `appending' to reopen a slot and updating
the slot contents.

    (window ()
      (@ stack (stack ()))
      (appending (@ stack)
        (para \"Message\")))

Here the `@' macro is used to access the app-local variable.
This sould be notice that app-local variable is related with
current `*app*'.

==============================================================
Elements
To make the `RYO.SHOES' DSL simply enough, I'll hide much of
the CSS in `/css/ryo-shoes.css'. Change that if you insisted
updating `RYO.SHOES' lookings.

 "
      :components
      (;;; Config
       ;; configure for Shoes
       ;; declaration for some closure variables
       (:file "config")
       (:file "utils")

       ;;; some low-level CLOG bindings
       (:file "shoes-server")

       ;;; Top-level Documentation
       (:file "documentation")
       (:file "common-implementation")

       ;;; Slots
       (:file "slot-creation")

       ;;; Elements
       (:file "element-creatation")

       ;; TextLike
       (:file "text-like-implementation")

       ;; app
       (:file "app-creatation")
       (:file "app-implementation")

       ))))))

;;; ryo.asd ends here
