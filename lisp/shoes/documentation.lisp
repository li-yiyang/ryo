;;; documentation.lisp --- Documentations for RYO.SHOES

;; File:        documentation.lisp
;; Description: Documentations for RYO.SHOES
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-08 16:12
;; Version: 0.0.0
;; Last-Updated: 2024-11-10 17:45
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

;; Check

(defclass check (element clog:clog-form-element) ()
  (:documentation
   "Check boxes are clickable square boxes than can be either
checked or unchecked. A single checkbox usually asks a \"yes\"
or \"no\" question. Sets of checkboxes are also seen in to-do
lists. "))

;; EditBox

(defclass edit-box (element clog:clog-text-area) ()
  (:documentation
   "Edit boxes are wide, rectangular boxes for entering text. On
the web, they call these textareas. These are multi-line edit
boxes for entering longer descriptions. Essays, even! "))

;; EditLine

(defclass edit-line (element clog:clog-form-element) ()
  (:documentation
   "Edit lines are a slender, little box for entering text. While
the EditBox is multi-line, an edit line is just one. Line,
that is. Horizontal, in fact. "))

;; Graphics

(defclass graphics (element clog:clog-canvas) ()
  (:documentation
   "Graphics is a box like thing in which you draw a lot of things. "))

;; Image

(defclass image (element clog:clog-img) ()
  (:documentation
   "An image is a picture in PNG, JPEG or GIF format. Shoes can
resize images or flow them in with text. Images can be loaded
from a file or directly off the web. "))

;; ListBox

(defclass list-box (element clog:clog-select)
  ((items :initform ()))
  (:documentation
   "List boxes (also called \"combo boxes\" or \"drop-down boxes\" or
\"select boxes\" in some places) are a list of options that
drop down when you click on the box. "))

;; Progress

(defclass progress (element clog:clog-progress-bar) ()
  (:documentation
   "Progress bars show you how far along you are in an activity.
Usually, a progress bar represents a percentage (from 0% to 100%.)
Shoes thinks of progress in terms of the decimal numbers 0.0 to 1.0. "))

;; Radio

(defclass radio (element clog:clog-form-element) ()
  (:documentation
   "Radio buttons are a group of clickable circles.
Click a circle and it'll be marked. Only one radio button can be
marked at a time. (This is similar to the ListBox, where only
one option can be selected at a time.) "))

;; Timer

(defclass timer-class (shoes)
  (thread
   (function :initarg :function
             :initform (error "Missing `:function'. ")))
  (:documentation
   "Shoes contains three timer classes:
1. the `animation' class;
2. the `every-sec' class;
3. and the `timer' class.

Both `animation' and `everies' loop over and over after they start.
`timer' happen once, as a one-shot timer. "))

(defclass animation (timer-class)
  ((fps :initarg :fps :initform 10))
  (:documentation
   "The `animation' class calls the `function' every `1/fps' second. "))

(defclass every-sec (timer-class)
  ((sec :initarg :sec :initform 10))
  (:documentation
   "The `every-sec' class calls the `function' every `sec' second. "))

(defclass timer (timer-class)
  ((sec :initarg :sec :initform 10))
  (:documentation
   "The `timer' class calls the `function' after `sec'. "))

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
     (defmacro ,name (&body texts*)
       ,(fmt "Create a ~A, ~A" name docstring)
       `(with-div-wrap-as-shoes (*slot* ,',name ,',(fmt "ryo-shoes-~(~A~)" name))
          ,@(expand-define-text-block-texts* texts*)))))

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
      (sub sub (clog:create-child
                *slot*
                (fmt "<sub class=\".ryo-shoes-sub\">~A</sub>" text)))))

(define-text-class sup
    "supscript text"
  (with-wrap-as-shoes
      (sub sub (clog:create-child
                *slot*
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

(defgeneric margin (shoes)
  (:documentation
   "Gets Shoes element's margin. "))

(defgeneric (setf margin) (margin shoes)
  (:documentation
   "Sets Shoes elements' margin. "))

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
The `handler' is handed `element' itself.
Return `element'. "))

(defmacro click (element &body body)
  "Set click event on `element'.
The `self' is the variable name to the clicked element. "
  `(on-click ,element (shoes-lambda ,element ,@body)))

(defgeneric on-change (element handler)
  (:documentation
   "Set change event on `element'.
Return `element'. "))

(defmacro change (element &body body)
  `(on-change ,element (shoes-lambda ,element ,@body)))

;;; Elements

;; Check

(defgeneric checked? (check)
  (:documentation
   "Returns whether the box is checked or not. So, `t' means
\"yes, the box is checked!\". "))

(defgeneric (setf checked?) (checked check)
  (:documentation
   "Marks or unmarks the `check' box.
Using setting to be `nil' for instance, unchecks the box."))

;; ListBox

(defgeneric choose (list-box item)
  (:documentation
   "Selects the option in the `list-box' that matches the string
given by `item'. "))

(defgeneric items (list-box)
  (:documentation
   "Returns the complete list of strings that the list box
presently shows as its options."))

;; Progress

(defgeneric fraction (progress)
  (:documentation
   "Returns a decimal number from 0.0 to 1.0,
indicating how far along the progress bar is. "))

(defgeneric (setf fraction) (decimal progress)
  (:documentation
   "Sets the progress to a decimal number between 0.0 and 1.0."))

;; Timer-Class

(defgeneric start (timer-class)
  (:documentation
   "Both types of timers automatically start themselves, so
there's no need to use this normally. But if you stop a timer
and would like to start it up again, then by all means: use
this! "))

(defgeneric stop (timer-class)
  (:documentation
   "Pauses the animation or timer. In the case of a one-shot
timer that's already happened, it's already stopped and this
method will have no effect. "))

(defgeneric toggle (timer-class)
  (:documentation
   "If the animation or timer is stopped, it is started.
Otherwise, if it is already running, it is stopped. "))

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
