;;;
;;; stage.lisp
;;;
;;; Denotes the area that ships can fly on.

(in-package #:rgbshift)

(defclass stage (node)
  ((width
    :initform 800)
   (height
    :initform 600)
   (color
    :initform '(255 255 255 255))))
