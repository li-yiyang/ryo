;;; documentation.lisp --- Documentations for RYO.SHOES

;; File:        documentation.lisp
;; Description: Documentations for RYO.SHOES
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-08 16:12
;; Version: 0.0.0
;; Last-Updated: 2024-11-08 16:12
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

;;;; Classes

;;; Shoes

(defclass shoes (clog:clog-obj)
  ((parent :initform *slot* :reader parent))
  (:documentation
   "The base class of Shoes component. "))

;;; App

(defclass app (shoes clog-gui:clog-gui-window)
  ((owner    :initform *app* :initarg :owner :reader owner)
   (bindings :initform (make-hash-table)))
  (:documentation
   "The App is the window itself.
Which may be closed or cleared and filled with new elements. "))

;;; Slots

(defclass slot (shoes)
  ((contents :initform ()))
  (:documentation
   "Slots are boxes used to lay out images, text and so on.
The two most common slots are stacks and flows. Slots can also
be referred to as \"boxes\" or \"canvases\" in Shoes terminology. "))

(defclass stack (slot clog:clog-div) ()
  (:documentation
   "A stack is simply a vertical stack of elements.
Each element in a stack is placed directly under the element preceding it.

A stack is also shaped like a box. So if a stack is given a width of 250,
that stack is itself an element which is 250 pixels wide.

To create a new stack, use the stack method, which is available inside any
slot. So stacks can contain other stacks and flows. "))

(defclass flow (slot clog:clog-div) ()
  (:documentation
   "A flow will pack elements in as tightly as it can. A width will be
filled, then will wrap beneath those elements. Text elements placed next
to each other will appear as a single paragraph. Images and widgets will
run together as a series.

Like the stack, a flow is a box. So stacks and flows can safely be embedded
and, without respect to their contents, are identical. They just treat
their contents differently.

Making a flow means calling the flow method. Flows may contain other flows
and stacks.

Last thing: The Shoes window itself is a flow."))

;;; Elements

(defclass element (shoes) ()
  (:documentation
   "The base class of Element component. "))

(defclass button (element clog:clog-button) ()
  (:documentation
   "Buttons are, you know, push buttons.

You click them and they do something. Buttons are known to
say \"OK\" or \"Are you sure?\" And, then, if you're sure,
you click the button.

Example:

    (button \"Hellow\")
    (button \"OK!\"
      (alert \"Yes\"))
"))

;; TextBlock
;; TODO:
;; make the declaration of new text-like element more easier

(defclass text-like (element) ()
  (:documentation
   "Elements that like the text, and should act like text.

There are two text-like element: `text-block' and `text-class'.
The `text-block' is backended to <div> in HTML; while `text-class'
is backended to <span> in HTML. "))

(defmacro define-text-block (name docstring)
  "Defines a subclass of `text-block' with `name' and `docstring'.
The div is bind with `.ryo-shoes-<name>' CSS style class. "
  `(progn
     (defclass ,name (text-block) () (:documentation ,docstring))
     (defmacro ,name (&rest texts)
       ,(fmt "Create a ~A, ~A" name docstring)
       `(with-div-wrap-as-shoes (*slot* ,',name ,',(fmt "ryo-shoes-~(~A~)" name))
          ,@(loop for text in texts collect `(%text-block ,text))))))

;; TODO: make the content more better data structure,
;; possible to learn how emacs take over the string
;; buffer? for now, it works and i don't see myself
;; rightnow have some urgent need to deal with texts
(defclass text-block (text-like clog:clog-div)
  ((contents :initform ()))
  (:documentation
   "The `text-block' object represents a group of text organized
as a single element.

The `text-block' is backended to <div> in HTML. "))

(define-text-block banner      "a 48 pixel font. ")
(define-text-block title       "a 34 pixel font. ")
(define-text-block subtitle    "a 26 pixel font. ")
(define-text-block tagline     "a 18 pixel font. ")
(define-text-block caption     "a 14 pixel font. ")
(define-text-block para        "a 12 pixel font. ")
(define-text-block inscription "a 10 pixel font. ")

(defclass text-class (text-like clog:clog-span) ()
  (:documentation
   "The `text-class' repersents a single piece of text. "))

(defmacro define-text-class (name docstring &body body)
  "Defines a subclass of `text-block' with `name' and `docstring'.
The span is bind with `.ryo-shoes-<name>' CSS style class. "
  `(progn
     (defclass ,name (text-block) () (:documentation ,docstring))
     (defun ,name (text)
       (declare (string text))
       ,(if (endp body)
            `(with-span-wrap-as-shoes (span ,name ,(fmt "ryo-shoes-~(~A~)" name))
               text)
            `(progn ,@body)))))

(define-text-class em     "emphasized, styled with italics")
(define-text-class span   "span in HTML, unstyled by default")
(define-text-class ins    "inserted text fragment, styled with a single underline. ")
(define-text-class link   "A link style text, with a single underline and colors
with a #06E (blue) colored stroke. ")
(define-text-class strong "strong text, styled in bold. ")

(define-text-class sub
    "subscript text"
  (with-wrap-as-shoes
      (sub sub (clog:create-child *slot*
                                  (fmt "<sub class=\".ryo-shoes-sub\">~A</sub>" text)))))

(define-text-class sup
    "supscript text"
  (with-wrap-as-shoes
      (sub sub (clog:create-child *slot*
                                  (fmt "<sup class=\".ryo-shoes-sub\">~A</sup>" text)))))

;;;; Protocol

;;; Common

(defgeneric parent (shoes)
  (:documentation
   "Gets the object for this element's container.

Also see the slot's `contents' to do the opposite:
get a container's elements."))

(defgeneric contents (shoes)
  (:documentation
   "A list of all children elements. "))

(defgeneric width (shoes)
  (:documentation
   "Gets Shoes window/element's width (in px). "))

(defgeneric (setf width) (width shoes)
  (:documentation
   "Sets Shoes window/element's width.

If `width' is
+ integer: unit in px
+ float between 0..1: converted into percents
+ string: raw HTML (not recommanded)"))

(defgeneric height (shoes)
  (:documentation
   "Gets Shoes window/element's height (in px). "))

(defgeneric (setf height) (height shoes)
  (:documentation
   "Sets Shoes window/element's height.

If `height' is
+ integer: unit in px
+ float between 0..1: converted into percents
+ string: raw HTML (not recommanded)"))

(defgeneric hide (shoes)
  (:documentation
   "Hides the element, so that it can't be seen.
See also `show' and `toggle'. "))

(defgeneric show (shoes)
  (:documentation
   "Reveals the element, if it is hidden.
See also `hide' and `toggle'. "))

(defgeneric toggle (shoes)
  (:documentation
   "Hides an element if it is shown.
Or shows the element, if it is hidden. "))

;; Events in Common

(defgeneric on-click (element handler)
  (:documentation
   "When a shoes `element' is clicked, its `handler' is called.
The `handler' is handed `element' itself. "))

;;; Elements

;; TextBlock

(defgeneric %text-block (obj)
  (:documentation
   "Turn `obj' into text-block like element to insert. "))

(defgeneric text (text-like)
  (:documentation
   "Gets/Sets a string of all of the characters in this text box.
Or, if the input object is not a `text-like' instance, return
a string that should represent the object in RYO.SHOES.

This will strip off any style or text classes and just return the
actual characters, as if seen on the screen. "))

(defgeneric (setf text) (string text-like)
  (:documentation
   "Sets the `string' of `text-block'.

Note that by setting the `text' of a nested `text-block',
you will clear the contents of the original `text-block',
and what you got is a single styled `text-block'. "))

;;; App

(defgeneric owner (app)
  (:documentation
   "Gets the app which launched this app.
In most cases, this will be nil. But if this app was launched using
the window method, the owner will be the app which called window."))

(defgeneric app-local-binding (app binding-sym)
  (:documentation
   "Get/Sets the local symbol binding within app.
The `binding-sym' shall be a symbol. "))

;;; documentation.lisp ends here
