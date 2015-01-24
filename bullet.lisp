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
   (width :initform (units 0.25))
   (height :initform (units 0.25))
   (direction
    :initarg :direction
    :initform (error ":direction must be specified.")
    :reader bullet-direction)
   (speed
    :initarg :speed
    :initform 800
    :reader bullet-speed)
   (lifespan
    :initarg :lifespan
    :initform 0.3
    :reader bullet-lifespan)
   (lifetime
    :initform 0.0
    :accessor bullet-lifetime)))

(defmethod initialize-instance :after ((bullet bullet) &key)
  (setf (bullet-color bullet) (slot-value bullet 'color)))

(defmethod update ((bullet bullet))
  (incf (bullet-lifetime bullet) *dt*)

  (move bullet (bullet-direction bullet) (* (bullet-speed bullet) *dt*))
  (when (> (bullet-lifetime bullet) (bullet-lifespan bullet))
    (remove-node (current-buffer) bullet)))

(defmethod draw ((bullet bullet))
  (with-slots (x y width height direction) bullet
    (draw-textured-rectangle-* x y 0 width height
			     (find-texture "data/square.png")
			     :vertex-color (bullet-color bullet))))

(defmethod collide ((bullet bullet) (wall wall))  
  (flet ((add-color-list (ll rl)
  	   (let ((return-value '()))
  	     (loop for i from 0 to (1- (length ll)) do
  		  (setf return-value (cons (+ (* 0.1 (nth i ll)) (+ (nth i rl))) return-value)))
  	     (reverse return-value))))
    (setf (wall-color wall) (add-color-list (bullet-color bullet) (wall-color wall))))
  (remove-node (current-buffer) bullet))
	 













