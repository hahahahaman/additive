;;;
;;; player.lisp
;;; 
;;; The player derived from a ship since it is a ship.
;;; File includes movement functions as well.

(in-package #:rgbshift)

;;; Keyboard press sensing functions

(defclass player (ship)
  ((height
    :initform (units 1)
    :type (unsigned-byte 8))
   (width
    :initform (units 1)
    :type (unsigned-byte 8))
   (color
    :initform '(0 0 0 255))
   (current-speed
    :initform 400.0
    :type short-float)
   (direction
    :initform 0.0
    :type short-float)
   (last-x
    :initform 0.0)
   (last-y
    :initform 0.0)
   (reload-cooldown
    :initform (make-cooldown :time 0.55)
    :type cooldown)
   (bullet-color-index
    :initform 0
    :type (unsigned-byte 2))
   (shift-cooldowns
    :initform (make-array 4
			  :element-type 'cooldown
			  :initial-contents (loop for i from 0 to 3
					      collect (make-cooldown :time 0.5))))))

(defgeneric fire (player))

(defmethod fire ((player player))
  (with-slots (direction x y width height bullet-color-index reload-cooldown) player
    (let ((bullet (make-instance 'bullet
				 :color (ecase bullet-color-index
					  (0 '(255.0 0 0 255.0))
					  (1 '(0 255.0 0 255.0))
					  (2 '(0 0 255.0 255.0)))				 
				 :direction direction))
	  (angle (+ (/ pi 2) direction))
	  (center-x (+ x (/ width 2)))
	  (center-y (+ y (/ height 2)))
	  (sqrt2 1.4142135))
      (insert bullet)
      
      ;; scaled so that bullet isn't in player hitbox
      (multiple-value-bind (new-x new-y) (rotate-point-ccw 0
							   (* sqrt2
							      (- (/ height -2) (bullet-size)))
							   angle)
	(move-to bullet
		 (+ center-x new-x)
		 (+ center-y new-y))))))

(defgeneric teleport (player shift-index))

(defmethod teleport ((player player) shift-index)
  (with-slots (last-x last-y x y direction bullet-color-index shift-cooldowns) player
    (setf bullet-color-index (mod (+ (ecase shift-index
				       (0 -1)
				       (1 -1)
				       (2 1)
				       (3 1))
				     bullet-color-index) 3)
	  (cooldown-timer (aref shift-cooldowns shift-index)) 0.0)
    (fire player)
    (setf last-x x last-y y)
    (move player (ecase shift-index
		   (0 (- direction (* 1/4 pi)))
		   (1 (- direction (* 3/4 pi)))
		   (2 (+ direction (* 3/4 pi)))
		   (3 (+ direction (* 1/4 pi))))
	  100)))

(defmethod update ((player player))
  (with-slots (direction
	       current-speed
	       last-x last-y
	       x y
	       color
	       width height			 
	       reload-cooldown
	       off-platform-cooldown
	       bullet-color-index
	       shift-cooldowns) player
    
    (setf direction (find-heading (+ x (/ width 2.0))
				  (+ y (/ height 2.0))
				  (screen-pointer-x) (screen-pointer-y))
	  last-x x last-y y)
    
    ;; holding up and the cursor is not in the player's bounding rectangle
    (when (and (holding-up-arrow)
	       (not (rect-in-rectangle-p
		     (cfloat (screen-pointer-x)) (cfloat (screen-pointer-y))
		     (cfloat 1) (cfloat 1)
		     (cfloat y) (cfloat x)
		     (cfloat width) (cfloat height))))
      (move player direction (* current-speed *dt*)))

    ;; adjust the camera so that player remains in the center
    ;; (let ((cam-diff-x (- (+ (/ width 2.0) x) (slot-value (current-buffer) 'window-x)))
    ;; 	  (cam-diff-y (- (+ (/ width 2.0) y) (slot-value (current-buffer) 'window-y))))
    ;;   (move-window-to (current-buffer)
    ;; 		      (slot-value (current-buffer) 'window-x)
    ;; 		      (cond ((< (- (/ *screen-height* 2.0) cam-diff-y) (/ *screen-height* 15.0))
    ;; 			     (slot-value (current-buffer) 'window-y))
    ;; 			    ((> cam-nndiff-y (/ *screen-height* 2.0))
    ;; 			     (+ (slot-value (current-buffer) 'window-y) (* (/ current-speed 1.5) *dt*)))
    ;; 			    ((< cam-diff-y (/ *screen-height* 2.0))
    ;; 			     (- (slot-value (current-buffer) 'window-y) (* (/ current-speed 1.5) *dt*)))))
    ;;   (move-window-to (current-buffer)
    ;; 		      (cond ((< (- (/ *screen-height* 2.0) cam-diff-x) (/ *screen-height* 15.0))
    ;; 			     (slot-value (current-buffer) 'window-x))
    ;; 			    ((> cam-diff-x (/ *screen-height* 2.0))
    ;; 			     (+ (slot-value (current-buffer) 'window-x) (* (/ current-speed 1.5) *dt*)))
    ;; 			    ((< cam-diff-x (/ *screen-height* 2.0))
    ;; 			     (- (slot-value (current-buffer) 'window-x) (* (/ current-speed 1.5) *dt*))))
    ;; 		      (slot-value (current-buffer) 'window-y)))
    
    ;; left mouse firing
    (when (< (cooldown-timer reload-cooldown) (cooldown-time reload-cooldown))
      (incf (cooldown-timer reload-cooldown) *dt*))

    (loop for cd across shift-cooldowns do
	 ;;(format t "~a~%" (cooldown-timer cd))
	 (when (< (cooldown-timer cd) (cooldown-time cd))
	   (incf (cooldown-timer cd) *dt*)))
    
    (cond ((and (> (cooldown-timer reload-cooldown) ;; left mouse
		   (cooldown-time reload-cooldown))
		(left-mouse-pressed))
	   (fire player)
	   (setf (cooldown-timer reload-cooldown) 0.0))
	  ((and (> (cooldown-timer (aref shift-cooldowns 0)) ;; q
		   (cooldown-time (aref shift-cooldowns 0)))
		(holding-up-left-arrow))
	   (teleport player 0))
	  ((and (> (cooldown-timer (aref shift-cooldowns 1)) ;; z
		   (cooldown-time (aref shift-cooldowns 1)))
		(holding-down-left-arrow))
	   (teleport player 1))
	  ((and (> (cooldown-timer (aref shift-cooldowns 2)) ;; c
		  (cooldown-time (aref shift-cooldowns 2)))
	    (holding-down-right-arrow))
	   (teleport player 2))
	  ((and (> (cooldown-timer (aref shift-cooldowns 3)) ;; e
		   (cooldown-time (aref shift-cooldowns 3)))
		(holding-up-right-arrow))
	   (teleport player 3)))
    (call-next-method)))

(defmethod draw ((player player))
  (with-slots (x y width height direction color
	       death-cooldown
	       death-reset-cooldown
	       death-reset-current-speed) player
    (draw-textured-rectangle-* x y 0 width height
			       (find-texture "up") ;;place holder image
			       :angle (radians->degrees (+ (/ pi 2) direction))
			       :tex-w (find-resource-property "up" :width)
			       :tex-h (find-resource-property "up" :height)
			       :clip-x 9 :clip-y 9 :clip-w 45 :clip-h 45
			       :vertex-color color)
    
    (when (> (cooldown-timer death-cooldown) 0.2)
      (let ((overlay-alpha (* 255 (/ (cooldown-timer death-cooldown)
				     (cooldown-time death-cooldown)))))	
	(draw-textured-rectangle 0 0 0 *width* *height*
				 (find-texture "data/dying-overlay.png")
				 :vertex-color `(0.0 0.0 0.0 ,overlay-alpha))))))

(defmethod collide ((player player) (wall wall))
  (with-slots (direction current-speed last-x last-y x y width height) player
    (format t "~a ~a~%" last-x last-y)
    ;; x movement pass
    (when (colliding-with-bounding-box-p 
	   wall
	   last-y x
	   (+ x width) (+ last-y height))
      (move-to player last-x y))

    ;; y movement pass
    (when (colliding-with-bounding-box-p
	   wall
	   y last-x
	   (+ last-x width) (+ y height))
      (move-to player x last-y))
    (when (> (abs (- last-x x)) 50.0)
      (move-to player last-x last-y))))

(defmethod collide ((player player) (stage stage))
  (call-next-method))
