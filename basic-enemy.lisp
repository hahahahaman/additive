;;;
;;; basic-enemy.lisp
;;;
;;; Let's try something

(in-package #:additive)

(defclass basic-enemy (ship)
  ((width
    :initform (units 2))
   (height
    :initform (units 2))
   (max-speed
    :initform 100.0
    :type short-float)
   (reload-cooldown
    :initform (make-cooldown :time 1.01)
    :type cooldown)
   (bullet-type
    :initform (random 3 (make-random-state t))
    :type (unsigned-byte 2))))

(defmethod die ((enemy basic-enemy))
  (with-slots (bullet-type max-speed) enemy
    (setf bullet-type (random 3))
    (call-next-method)
    (setf max-speed 100)))

(defmethod fire ((enemy basic-enemy))
  (with-slots (direction x y width height bullet-type reload-cooldown) enemy
    (let ((bullet (make-instance 'bullet
				 :color (enum-rgb bullet-type)				 
				 :direction direction))
	  (angle (+ (/ pi 2) direction))
	  (center-x (+ x (/ width 2)))
	  (center-y (+ y (/ height 2)))
	  (sqrt2 1.4142135))
      (insert bullet)
      
      ;; scaled so that bullet isn't in player hitbox
      (multiple-value-bind (new-x new-y)
	  (rotate-point-ccw 0
			    (* sqrt2 (- (/ height -2) (bullet-size)))
			    angle)
	(move-to bullet
		 (+ center-x new-x)
		 (+ center-y new-y))))
    
    (ecase bullet-type
      (0 (play-sample "beepr.wav"))
      (1 (play-sample "beepg.wav"))
      (2 (play-sample "beepb.wav")))))

(defmethod update ((enemy basic-enemy))
  (with-slots (x y width height direction death-cooldown reload-cooldown bullet-type max-speed
		 stun-lock color) enemy
    (if (>= (cooldown-timer stun-lock) (cooldown-time stun-lock))
	(progn
	  (let* ((stage (slot-value (current-buffer) 'stage))
		 (player (slot-value (current-buffer) 'player))
		 (e-center-x (+ x (/ width 2)))
		 (e-center-y (+ y (/ height 2)))
		 (s-center-x (+ (slot-value stage 'x) (/ (slot-value stage 'width) 2)))
		 (s-center-y (+ (slot-value stage 'y) (/ (slot-value stage 'height) 2)))
		 (p-x (slot-value player 'x))
		 (p-y (slot-value player 'y)))

	    (cond ((not (colliding-with-p enemy stage)) ;;first: get on stage
		   (setf direction (find-heading e-center-x e-center-y s-center-x s-center-y)))
		  ((> (distance-between enemy player)
		      (+ (random (- 500 100) (make-random-state t)) 100)) ;; second : get to player
		   (setf direction (find-heading e-center-x e-center-y p-x p-y)))
		  ((and (> (cooldown-timer reload-cooldown) ;;third : attack
			   (cooldown-time reload-cooldown))
			(< (distance-between enemy player) 100))
		   (setf direction (find-heading e-center-x e-center-y p-x p-y))
		   (fire enemy)
		   (setf (cooldown-timer reload-cooldown) 0.0))))

	  (move enemy direction (* (* max-speed (- 1 (/ (nth 1 color) 255.0))) *dt*)))
	(incf (cooldown-timer stun-lock) *dt*))
    
    (when (< (cooldown-timer reload-cooldown) (cooldown-time reload-cooldown))
      (incf (cooldown-timer reload-cooldown) *dt*)))
  (call-next-method))

(defmethod draw ((enemy basic-enemy))
  (with-slots (x y width height direction color) enemy
    ;;(draw-world-to-screen #'draw-box x y width height)
    (draw-textured-rectangle-* x y 0 width height
			       (find-texture "up") ;;place holder image
			       :angle (radians->degrees (+ (/ pi 2) direction))
			       :tex-w (find-resource-property "up" :width)
			       :tex-h (find-resource-property "up" :height)
			       :clip-x 9 :clip-y 9 :clip-w 45 :clip-h 45
			       :vertex-color color)))

