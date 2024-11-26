;;; c3-plot.lisp --- CLOG-C3 Plot wrapper for RYO.SHOES

;; File:        c3-plot.lisp
;; Description: CLOG-C3 Plot wrapper for RYO.SHOES
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-22 18:07
;; Version: 0.0.0
;; Last-Updated: 2024-11-22 18:07
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.shoes)

(defclass c3-plot (clog-c3:clog-c3 element)
  (id)
  (:documentation
   "A CLOG-C3 plot.

Use `c3-plot' macro to make a C3 plot on the "))

(defmethod (setf width) (width (plot c3-plot))
  (clog:set-geometry plot :width width))

(defmethod (setf height) (height (plot c3-plot))
  (clog:set-geometry plot :height height))

(defun %c3-plot (&key width height (fill-slot-p t) &allow-other-keys)
  "Create a CLOG-C3 plot. "
  (let ((id (gensym "C3-PLOT")))
    ;; init with a dummy data
    (with-wrap-as-shoes
        (c3-plot c3-plot (clog-c3:create-clog-c3-plot *slot* ()
                                                      :width  300
                                                      :height 300
                                                      :id     id))
      (setf (slot-value c3-plot 'id) id)
      (cond (fill-slot-p
             (clog-gui:set-on-window-size
              *app*
              (let ((slot *slot*))
                (lambda (obj) (declare (ignore obj))
                  (let ((*slot* slot))
                    (clog:set-geometry c3-plot
                                       :width  (width  *slot*)
                                       :height (height *slot*)))))))
            (t
             (when width  (setf (width  c3-plot) width))
             (when height (setf (height c3-plot) height))))
      ;; the dummy data should be hide
      (clog-c3:c3-hide c3-plot id :with-legend nil))))

(defmacro c3-plot ((&rest styles &key width height (fill-slot-p t) &allow-other-keys)
                   &body body)
  "Make a CLOG-C3 plot element. "
  (declare (ignore width height fill-slot-p))
  `(with-wrap (*self* (%c3-plot ,@styles))
     (flet ((plot (data &key color
                        (id   (clog-c3:c3-data-id data))
                        (type (clog-c3:c3-data-type data)))
              (clog-c3:c3-load *self* data :color color :id id :type type)))
       ,@body)))

;;; c3-plot.lisp ends here
