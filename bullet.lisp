;;;
;;; bullet.lisp
;;;
;;; rgb colored bullets fired from ships

(in-package #:additive)

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
   (lifespan
    :initarg lifespan
    :initform (make-cooldown :time 0.4)
    :type cooldown)))

(defgeneric bullet-collision (bullet node))

(defmethod bullet-collision ((bullet bullet) (node node))
  (with-slots (color) node
    (setf color (add-rgb-color-list (bullet-color bullet) color)))
  (remove-node (current-buffer) bullet))

(defmethod initialize-instance :after ((bullet bullet) &key)
  (setf (bullet-color bullet) (slot-value bullet 'color)))

(defmethod update ((bullet bullet))
  (with-slots (lifespan) bullet
    (move bullet (bullet-direction bullet) (* (bullet-current-speed bullet) *dt*))
    (incf (cooldown-timer lifespan) *dt*)
    (when (> (cooldown-timer lifespan) (cooldown-time lifespan))
      (remove-node (current-buffer) bullet))))

(defmethod draw ((bullet bullet))
  (with-slots (x y width height direction) bullet
    (draw-textured-rectangle-* x y 0 width height
			     (find-texture "square.png")
			     :vertex-color (bullet-color bullet))))

(defmethod collide ((bullet bullet) (wall wall))  
  (bullet-collision bullet wall))

(defmethod collide ((bullet bullet) (ship ship))
  (bullet-collision bullet ship)
  (with-slots (direction color) bullet
    (with-slots (knock-back knock-back-direction knock-back-speed stun-lock max-speed) ship
      (let ((bullet-type (loop for i from 0 to 2 when (= (nth i color) 255) do (return i))))
	(case bullet-type
	  (0 (setf knock-back-direction direction		  
		   (cooldown-timer knock-back) 0.0)
	     (incf (cooldown-time knock-back)
		   (* 0.1 (/ (nth 0 (slot-value ship 'color)) 255))))
	  (1 t)
	  (2 (incf (cooldown-time stun-lock) 0.05)
	     (setf (cooldown-timer stun-lock) 0.0)))))))












