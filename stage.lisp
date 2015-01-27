;;;
;;; stage.lisp
;;;
;;; Denotes the area that ships can fly on.

(in-package #:rgbshift)

(defclass stage (node)
  ((x
    :initform 0.0
    :type single-float)
   (y
    :initform 0.0
    :type single-float)
   (width
    :initform 1000.0
    :type single-float)
   (height
    :initform 1000.0
    :type single-float)
   (color
    :initform '(255 255 255 255)
    :type list)))

(defmethod update ((stage stage))
  (call-next-method)) 

(defmethod draw ((stage stage))
  (with-slots (x y width height color) stage
      (draw-world-to-screen #'draw-box x y width height :color color)))
