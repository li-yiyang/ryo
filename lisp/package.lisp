;;; package.lisp --- Package definition for RYO

;; File:        package.lisp
;; Description: Package definition for RYO
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-10-31 10:11
;; Version: 0.0.0
;; Last-Updated: 2024-11-09 14:35
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

 ")
  (:export
   ;; config
   #:*shoes-port*
   #:*shoes-title*

   ;; closures
   #:*app*
   #:*slot*
   #:*self*

   ;; App
   #:window

   ;; Slots
   #:stack
   #:flow

   ;;; Elements
   #:button
   #:check
   #:edit-box
   #:edit-line
   #:graphics
   #:image
   #:list-box
   #:progress
   #:radio

   ;; timer-class
   #:animation
   #:every-nsec
   #:timer

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

   ;; built-in method
   #:alert

   ;; shoes server
   #:shutdown-shoes
   #:boot-shoes
   #:reboot-shoes
   ))

(defpackage #:ryo
  (:use :cl :ryo.macros))

;;; package.lisp ends here
