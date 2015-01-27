;;;
;;; abyss.lisp
;;;
;;; The part of the level that isn't the stage.

(in-package #:rgbshift)

(defclass abyss (node)
  ((x
    :initarg x
    :initform 0
    :type single-float)
   (y
    :initarg y
    :initform 0
    :type single-float)
   (width
    :initarg width
    :initform 3000.0
    :type single-float)
   (height
    :initarg height
    :initform 3000.0
    :type single-float)))

(defmethod update ((abyss abyss))
  (call-next-method))

(defmethod draw ((abyss abyss))
  (with-slots (x y width height) abyss
    (draw-world-to-screen #'draw-textured-rectangle x y 0 width height (find-texture "data/space.png"))))
