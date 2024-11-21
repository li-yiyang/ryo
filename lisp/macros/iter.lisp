;;; iter.lisp --- Macros for iteration

;; File:        iter.lisp
;; Description: Macros for iteration
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-10-31 10:58
;; Version: 0.0.0
;; Last-Updated: 2024-10-31 10:58
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/ryo
;; Keywords:
;; Compatibility:
;;
;;

(in-package :ryo.macros)

(defmacro iter-i* (bindings &body body)
  "Do iteration by bindings.

Syntax:
The `bindings' each element should be like:

    (var end)              ;
    (var start end)
    (var start step end)
    (var start step end :reject expr . more-keys)
    (var start end . more-keys)

Possible keys:
+ `:reject': rejct on condition
+ `:accept': accept on condition

Example:

   (iter-i* ((i 10 :reject (evenp i)))
     (fmt! \"~A~%\" i))
"
  (loop for binding in bindings
        for (def keys) = (multiple-value-list
                          (split-list-plist?-by-literal-keyword binding))
        for (var a b c) = def
        for start  = (if b a 0)
        for end    = (or c b a)
        for step   = (if c b 1)

        for endp   = (cond ((and (numberp step))
                            (if (> step 0)     `(<= ,var ,end) `(>= ,var ,end)))
                           ((and (numberp start) (numberp end))
                            (if (<= start end) `(<= ,var ,end) `(>= ,var ,end)))
                           (t `(:if (> ,step 0) (<= ,var ,end) (>= ,var ,end))))

        for reject = (getf keys :reject)
        for accept = (getf keys :accept)

        if reject
          collect reject into reject-conditions
        if accept
          collect accept into accept-conditions

        collect `(:for ,var = ,start :then (incf ,var ,step) :while ,endp)
          into var-inits

        finally
           (return
             `(loop ,@(apply #'nconc var-inits)
                    :do (,@(cond
                             ;; none condition
                             ((and (endp reject-conditions)
                                   (endp accept-conditions))
                              '(progn))
                             ;; only accept
                             ((and (endp reject-conditions)
                                   (not (endp accept-conditions)))
                              `(when (and ,@accept-conditions)))
                             ;; only reject
                             ((and (not (endp reject-conditions))
                                   (endp accept-conditions))
                              `(when (not (or ,@reject-conditions))))
                             ;; both
                             (t
                              `(when (and (not (or ,@reject-conditions))
                                          ,@accept-conditions))))
                         ,@body)))))

(defmacro dotimes-collect ((i n &optional (collect (gensym "LIST"))) &body body)
  "Do `n' times on var `i' and collect `body' results and return it.
The `collect' is the final return symbol, usually not setted.

Behind the scene is a `dotimes' with a list named `collect', the
result of `body' will be pushed into the `collect' list and return
reversed `collect' in the end.

NOTE: To use `collect' directly during the iteration is possible,
though it may be tricky and may not work as expected. "
  (declare (symbol i collect))
  `(let ((,collect ()))
     (dotimes (,i ,n (nreverse ,collect))
       (push (progn ,@body) ,collect))))

(defmacro map* (type (var-sequence &rest more-var-sequences) &body body)
  "A simple map DSL.

Example:

  (map* (list1 list2 (lst3 list3))
    (list list1 list2 lst3))
"
  (loop for var-seq in (cons var-sequence more-var-sequences)
        for var = (if (listp var-seq) (first  var-seq) var-seq)
        for seq = (if (listp var-seq) (second var-seq) var-seq)
        collect var into vars
        collect seq into seqs
        finally (return `(map ,type (lambda ,vars ,@body) ,@seqs))))

(defmacro mapcar* ((var-sequence &rest more-var-sequences) &body body)
  "A simple mapcar DSL.

Example:

  (map* (list1 list2 (lst3 list3))
    (list list1 list2 lst3))
"
  (loop for var-seq in (cons var-sequence more-var-sequences)
        for var = (if (listp var-seq) (first  var-seq) var-seq)
        for seq = (if (listp var-seq) (second var-seq) var-seq)
        collect var into vars
        collect seq into seqs
        finally (return `(mapcar (lambda ,vars ,@body) ,@seqs))))

(defmacro indexed-map* (type (id var-sequence &rest more-var-sequences) &body body)
  "Like `map*', but with an extra counter `id' for nth iter. "
  `(let ((,id 0))
     (map* ,type (,var-sequence ,@more-var-sequences) ,@body (incf ,id))))

(defmacro indexed-mapcar* ((id var-sequence &rest more-var-sequences) &body body)
  "Like `mapcar*', but with an extra counter `id' for nth iter. "
  `(let ((,id 0)) (mapcar* (,var-sequence ,@more-var-sequences) ,@body (incf ,id))))

;;; iter.lisp ends here
