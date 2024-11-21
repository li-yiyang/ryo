;;; histogram.lisp --- Documentation and Protocol of Histogram

;; File:        histogram.lisp
;; Description: Documentation and Protocol of Histogram
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-15 18:05
;; Version: 0.0.0
;; Last-Updated: 2024-11-15 18:05
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.stat)

(defclass histogram ()
  ((bins
    :initform ()
    :initarg  :bins
    :accessor hist-bins
    :documentation
    "Each counter bin number as a list.
Using `hist-bin' to get/set specific dimension bin number.
Using `hist-bins' to set. ")
   (mins
    :initform ()
    :initarg  :mins
    :accessor hist-mins
    :documentation
    "Each counter bin dimension min as a list.
Using `hist-min' to get/set specific dimension min value.
Using `hist-mins' to set. ")
   (maxs
    :initform ()
    :initarg  :maxs
    :accessor hist-maxs
    :documentation
    "Each counter bin dimension max as a list.
Using `hist-max' to get/set specific dimension max value.
Using `hist-maxs' to set. ")
   (dims
    :initarg  :dims
    :initform nil
    :reader   hist-dims
    :documentation
    "Dimension of each counter bin. ")
   (counts
    :documentation
    "The N-dimension array for the histogram counts.
This should only be used internally.

Don't mess up with method `hist-counts' which counts
for the total bined data within `counts'. ")
   (buffer
    :initform ()
    :documentation
    "Data added to histogram will be stored in buffer.
This makes sure that histogram could be `hist-rebin!'.
Use `hist-clean-buffer!' to clear data in buffer. ")
   (ticks
    :reader hist-ticks
    :documentation
    "Each counter bin dimension ticks as a list.
Not recommanded to use directly. "))
  (:documentation
   "Histogram for any dimensional data.

Use `make-histogram' function to generate a histogram from data.
"))

(defgeneric hist-bin (hist i)
  (:documentation
   "Get/Sets `i' th dimension bin number. "))

(defgeneric hist-min (hist i)
  (:documentation
   "Get/Sets `i' th dimension min value. "))

(defgeneric hist-max (hist i)
  (:documentation
   "Get/Sets `i' th dimension max value. "))

(defgeneric hist-rebin! (hist)
  (:documentation
   "Rebin on histogram `hist' with data in `buffer'. "))

(defgeneric hist-dump (hist)
  (:documentation
   "Dump a new histogram from `hist'.
The `hist' data `buffer' would not be copied. "))

(defgeneric hist-data-count (hist)
  (:documentation
   "Totoal number count within `hist' counts. "))

(defgeneric add-to-hist (hist data)
  (:documentation
   "Add data to histogram `hist'. "))

(defun make-histogram (data &key dim min max (bin 10))
  "Make a histogram from data.
Return a histogram instance.

Para:
+ if value is `nil' then the parameter will be automatically
  calculated from the input data;
+ `dim': int or int list
  if `nil', calculate by first element if data is not empty,
  otherwise, assuming `dim' is 1;
+ `min', `max': int or int list
  if `nil' calculate by data (each dimension min or max);
  if `data' is empty, `max' and `min' will be 1 and -1.
"
  (cond ((endp data)
         (make-instance 'histogram :dims (or dim 1)
                                   :maxs (or max 1)
                                   :mins (or min -1)
                                   :bins bin))
        ;; ensure that data is in shape of list of elements list
        ((not (listp (first data)))
         (make-histogram (mapcar #'list data) :dim 1
                                              :min min
                                              :max max
                                              :bin bin))
        ((not dim)
         (make-histogram data :dim (length (first data))
                              :min min
                              :max max
                              :bin bin))
        (t
         (let* ((trans (apply #'mapcar #'list data))
                (max   (or max (mapcar (lambda (col) (reduce #'max col)) trans)))
                (min   (or min (mapcar (lambda (col) (reduce #'min col)) trans))))
           (let ((hist (make-instance 'histogram :dims dim
                                                 :maxs max
                                                 :mins min
                                                 :bins bin)))
             (dolist (dat data hist) (add-to-hist hist dat)))))))

;;; histogram.lisp ends here
