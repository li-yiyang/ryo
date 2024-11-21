;;; histogram-implementation.lisp --- Implementation for histogram

;; File:        histogram-implementation.lisp
;; Description: Implementation for histogram
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-15 20:17
;; Version: 0.0.0
;; Last-Updated: 2024-11-15 20:17
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.stat)

(defmethod initialize-instance :after ((hist histogram) &key)
  (with-slots (bins mins maxs dims) hist
    (flet ((listfy (val dim)
             (cond ((listp val)
                    (assert (length= val dim))
                    val)
                   (t (make-list dim :initial-element val))))
           (len (lst)
             (if (listp lst) (length lst) 1)))
      ;; assert for bins > 0
      (if (listp bins)
          (dolist (bin bins) (assert (> bin 0)))
          (assert (> bins 0)))
      ;; calculate the dimension
      (setf dims (or dims (max (len mins) (len maxs) (len bins))))
      ;; sets the min and max
      (setf mins (listfy mins dims)
            maxs (listfy maxs dims)
            bins (listfy bins dims))
      ;; rebin
      (hist-rebin! hist))))

(defmethod (setf hist-bins) :around (bins-list (hist histogram))
  (with-slots (dims) hist
    (assert (length= bins-list dims))
    (call-next-method)
    (hist-rebin! hist)))

(defmethod (setf hist-mins) :around (mins-list (hist histogram))
  (with-slots (dims) hist
    (assert (length= mins-list dims))
    (call-next-method)
    (hist-rebin! hist)))

(defmethod (setf hist-maxs) :around (maxs-list (hist histogram))
  (with-slots (dims) hist
    (assert (length= maxs-list dims))
    (call-next-method)
    (hist-rebin! hist)))

(defmethod hist-bin ((hist histogram) i)
  (nth i (slot-value hist 'bins)))

(defmethod hist-min ((hist histogram) i)
  (nth i (slot-value hist 'mins)))

(defmethod hist-max ((hist histogram) i)
  (nth i (slot-value hist 'maxs)))

(defmethod (setf hist-bin) (i-th-bin (hist histogram) i)
  (with-slots (dims bins) hist
    (assert (< i dims))
    (setf (nth i bins) dims)
    (hist-rebin! hist)))

(defmethod (setf hist-min) (i-th-min (hist histogram) i)
  (with-slots (dims mins) hist
    (assert (< i dims))
    (setf (nth i mins) dims)
    (hist-rebin! hist)))

(defmethod (setf hist-max) (i-th-max (hist histogram) i)
  (with-slots (dims maxs) hist
    (assert (< i dims))
    (setf (nth i maxs) dims)
    (hist-rebin! hist)))

(defmethod hist-rebin! ((hist histogram))
  (with-slots (bins mins maxs dims ticks counts buffer) hist
    (setf ticks (mapcar* (maxs mins bins) (float (/ (- maxs mins) bins))))
    (setf counts (make-array dims))
    (dotimes (i dims)
      (setf (aref counts i)
            (make-array (nth i bins) :initial-element 0)))
    (let ((temp buffer))
      (setf buffer ())
      (dolist (data temp)
        (add-to-hist hist data)))
    hist))

(defmethod hist-dump ((hist histogram))
  (with-slots (bins mins maxs dims ticks counts buffer) hist
    (let ((new (make-instance 'histogram :dims dims
                                         :mins mins
                                         :maxs maxs
                                         :bins bins))
          (new-cnts (make-array dims)))
      (setf (slot-value new 'counts)
            (dotimes (i dims new-cnts)
              (setf (aref new-cnts i) (alexandria:copy-array new-cnts))))
      (setf (slot-value new 'buffer) buffer)
      (setf (slot-value new 'ticks)  ticks)
      new)))

(defmethod add-to-hist ((hist histogram) (data number))
  (add-to-hist hist (list data)))

(defmethod add-to-hist ((hist histogram) (data list))
  (with-slots (counts mins maxs ticks dims) hist
    (assert (length= data dims))
    (let ((dim 0))
      (map 'nil
           (lambda (min val max tick)
             (when (and (<= min val) (< val max))
               (incf (aref (aref counts dim) (floor (- val min) tick))))
             (incf dim))
           mins data maxs ticks))))

(defmethod add-to-hist :around ((hist histogram) data)
  (call-next-method)
  (push data (slot-value hist 'buffer)))

(defmethod hist-data-count ((hist histogram))
  (with-slots (counts) hist
    (reduce (lambda (count-dim) (reduce #'+ count-dim)) counts)))

;;; histogram-implementation.lisp ends here
