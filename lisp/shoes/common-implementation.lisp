;;; common-implementation.lisp --- Common Implementations

;; File:        common-implementation.lisp
;; Description: Common Implementations
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-08 16:27
;; Version: 0.0.0
;; Last-Updated: 2024-11-08 16:27
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

(defmethod contents ((slot slot))
  (reverse (slot-value slot 'contents)))

(defmethod width ((shoes shoes))
  (clog:width shoes))

(defmethod height ((shoes shoes))
  (clog:height shoes))

(defmethod (setf width) ((width integer) (shoes shoes))
  (cond ((> width 0) (setf (clog:width shoes) width))
        ((< width 0)
         (if (parent shoes)
             (setf (clog:width shoes) (+ (width (parent shoes)) width))
             (errorf "Trying to set root element ~A with negative width ~D" shoes width)))
        (t (errorf "Trying to set ~A width to 0. " shoes))))

(defmethod (setf height) ((height integer) (shoes shoes))
  (cond ((> height 0)
         (setf (clog:height shoes) height))
        ((< height 0)
         (if (parent shoes)
             (setf (clog:height shoes) (+ (height (parent shoes)) height))
             (errorf "Trying to set root element ~A with negative height ~D" shoes height)))
        (t (errorf "Trying to set ~A width to 0. " shoes))))

(defmethod hide ((shoes shoes))
  (setf (clog:visiblep shoes) nil))

(defmethod show ((shoes shoes))
  (setf (clog:visiblep shoes) t))

(defmethod toggle ((shoes shoes))
  (setf (clog:visiblep shoes) (not (clog:visiblep shoes))))

;;; Events

(defmethod on-click ((element element) handler)
  (clog:set-on-click element (ignore-obj obj (funcall handler)))
  element)

(defmethod on-change ((element element) handler)
  (clog:set-on-change element (ignore-obj obj (funcall handler)))
  element)

;;; common-implementation.lisp ends here
