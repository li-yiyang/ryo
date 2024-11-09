;;; elements-creation.lisp --- Macros and Functions to create Shoes elements

;; File:        elements-creation.lisp
;; Description: Macros and Functions to create Shoes elements
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-05 18:09
;; Version: 0.0.0
;; Last-Updated: 2024-11-05 18:09
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

;; (defmacro animate ((frame &key (fps 10)) &body body)
;;   "Starts an animation timer.

;; The animation timer runs parallel to the rest of the app.
;; The `fps' is an number, the frames per seconds. This number
;; dictates how many times per second the attached block will
;; be called.

;; The block is given a `frame' number. Starting with zero, the
;; `frame' number tells the block how many frames of the animation
;; have been shown.

;; If no number is given, the `fps' defaults to `10'. "
;;   ())

;; (defmacro background (pattern))

;; (defmacro border (text &key strokewidth))

(defmacro button (text &body body)
  "Adds a push button with the message `text' written
across its surface. An optional block can be attached, which
is called if the button is pressed. "
  (let ((button (gensym "BUTTON")))
    `(let ((,button (clog:create-button *slot* :content ,text)))
       (change-class ,button 'button)
       (clog:set-on-click ,button
                          (lambda (,button)
                            (declare (ignore ,button))
                            ,@body))
       ,button)))

;; (defun caption (text))

;; (defun check ())

;; (defun code (text))

;; (defun del (text))

;; (defun dialog ((&rest styles) &body body)
;;   "Opens a new app window (just like `window' method does),
;; but the window is given a dialog box look. ")

;; (defun edit-box (text))

;; (defun edit-line (text))

;; (defun em (text))

(defmacro every ((count &key (seconds 1)) &body body)
  "A timer similar to the animation method, but much slower.
This timer fires a given number of seconds, running the block
attached. So, for example, if you need to check a web site every
five minutes, you'd call every(300) with a block containing
the code to actually ping the web site."
  )

(defmacro flow ((&rest styles &key width height) &body body)
  "A flow is an invisible box (or \"slot\") in which you place
Shoes elements. Both flows and stacks are explained in great
detail on the main Slots page.

Flows organize elements horizontally. Where one would use a stack
to keep things stacked vertically, a flow places its contents
end-to-end across the page. Once the end of the page is reached,
the flow starts a new line of elements."
  (declare (ignorable styles))
  `(let ((*slot* (clog:create-div *slot* :class "ryo-shoes-flow")))
     (change-class *slot* 'flow)
     ,(when width  `(setf (clog:width  *slot*) (fmt "~Dpx" ,width)))
     ,(when height `(setf (clog:height *slot*) (fmt "~Dpx" ,height)))
     ,@body
     *slot*))

;; (defun image (path))

;; (defun imagesize (path))

;; (defun ins (text))

;; (defmacro link (text &body body))

;; (defmacro list-box (&rest items))

;; (defun progress ())

;; (defun para (text))

;; (defun span (text))

(defmacro stack ((&rest styles &key width height) &body body)
  "Creates a new stack. A stack is a type of slot.
(See the main Slots page for a full explanation of both stacks and flows.)

In short, stacks are an invisible box (a \"slot\") for placing stuff.
As you add things to the stack, such as buttons or images, those things
 pile up vertically. Yes, they stack up!"
  (declare (ignorable styles))
  `(let ((*slot* (clog:create-div *slot* :class "ryo-shoes-stack")))
     (change-class *slot* 'stack)
     ,(when width  `(setf (clog:width  *slot*) (fmt "~Dpx" ,width)))
     ,(when height `(setf (clog:height *slot*) (fmt "~Dpx" ,height)))
     ,@body
     *slot*))

;; (defun sub (text))

(defmacro subtitle (&rest texts)
  "Makes a subtitle from a list of `texts'. "
  `(let ((*slot* (clog:create-div *slot* :class "ryo-shoes-subtitle")))
     (change-class *slot* 'subtitle)
     ,@(loop for text in texts
             collect `(text-block ,text))
     *slot*))

;; (defun sup (text))

;; (defun tagline (text))

(defun make-timer (second function)
  "Create a timer that run `function' after `second'. "
  (let ((timer (make-instance 'timer :function function)))
    (with-slots (function) timer
      (bt:make-thread (lambda () (sleep second) (funcall function))))))

(defmacro timer (seconds &body body)
  "A one-shot timer. If you want to schedule to run some code in a
few seconds (or minutes, hours) you can attach the code as a block here."
  `(make-instance 'timer :second ,seconds :function (lambda () ,@body)))

(defun text-block (text)
  "Makes a span from `text'.
If `text' is a `text-block' sub class element,
return `text' directly. "
  (if (typep text 'text-block) text
      (let ((span (clog:create-span *slot* :content (fmt "~A" text))))
        (change-class span 'text-block)
        span)))

(defmacro title (&rest texts)
  "Makes a title from a list of `texts'. "
  `(let ((*slot* (clog:create-div *slot* :class "ryo-shoes-title")))
     (change-class *slot* 'title)
     ,@(loop for text in texts
             collect `(text-block ,text))
     *slot*))

;; (defun video (path-or-url))

;; (defmacro window ((&rest styles) &body body))

;;; elements-creation.lisp ends here
