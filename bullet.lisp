;;;
;;; bullet.lisp
;;;
;;; rgb colored bullets fired from ships

(in-package #:rgbshift)

(defclass bullet (node)
  ((color
    :initarg :color
    :initform (error ":color must be specified.")
    :accessor bullet-color)
   (width
    :initarg width
    :initform (bullet-size))
   (height
    :initarg height
    :initform (bullet-size))
   (direction
    :initarg :direction
    :initform (error ":direction must be specified.")
    :reader bullet-direction)
   (current-speed
    :initarg :speed
    :initform 600.0
    :type single-float
    :reader bullet-current-speed)
   (lifespan-cooldown
    :initarg lifespan-cooldown
    :initform (make-cooldown :time 0.2)
    :type cooldown)))

(defmethod initialize-instance :after ((bullet bullet) &key)
  (setf (bullet-color bullet) (slot-value bullet 'color)))

(defmethod update ((bullet bullet))
  (with-slots (lifespan-cooldown) bullet
    (move bullet (bullet-direction bullet) (* (bullet-current-speed bullet) *dt*))
    (incf (cooldown-timer lifespan-cooldown) *dt*)
    (when (> (cooldown-timer lifespan-cooldown) (cooldown-time lifespan-cooldown))
      (remove-node (current-buffer) bullet))))

(defmethod draw ((bullet bullet))
  (with-slots (x y width height direction) bullet
    (draw-textured-rectangle-* x y 0 width height
			     (find-texture "data/square.png")
			     :vertex-color (bullet-color bullet))))

(defmethod collide ((bullet bullet) (wall wall))  
  (setf (wall-color wall) (add-rgb-color-list (bullet-color bullet) (wall-color wall)))
  (remove-node (current-buffer) bullet))
	 













