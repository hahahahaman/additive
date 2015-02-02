;;;
;;; player.lisp
;;; 
;;; The player derived from a ship since it is a ship.
;;; File includes movement functions as well.

(in-package #:additive)

;;; Keyboard press sensing functions

(defclass player (ship)
  ((height
    :initform (units 1.5)
    :type (unsigned-byte 8))
   (width
    :initform (units 1.5)
    :type (unsigned-byte 8))
   (color
    :initform '(0 0 0 255))
   (max-speed
    :initform 300.0
    :type short-float)
   (direction
    :initform 0.0
    :type short-float)
   (last-x
    :initform 0.0)
   (last-y
    :initform 0.0)
   (reload-cooldown
    :initform (make-cooldown :time 1.01)
    :type cooldown)
   (bullet-type
    :initform (random 3 (make-random-state))
    :type (unsigned-byte 2))
   (shift-cooldowns
    :initform (make-array 4
			  :element-type 'cooldown
			  :initial-contents (loop repeat 4
					       collect (make-cooldown :time 3.13))))
   (shift-color
    :initform (make-array 4
			  :element-type 'fixnum
			  :initial-contents '(-1 1 -1 1)))
   (shift-dir
    :initform (make-array 4
			  :element-type 'double-float
			  :initial-contents `(,(- (* 1/4 pi))
					      ,(- (* 3/4 pi))
					      ,(* 3/4 pi)
					      ,(* 1/4 pi))))))

(defmethod fire ((player player))
  (with-slots (direction x y width height bullet-type reload-cooldown) player
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

(defgeneric teleport (player shift-index))

(defmethod teleport ((player player) shift-index)
  (with-slots (last-x last-y x y direction bullet-type
		      shift-cooldowns shift-color shift-dir) player
    (setf bullet-type (mod (+ bullet-type (aref shift-color shift-index)) 3)
	  (cooldown-timer (aref shift-cooldowns shift-index)) 0.0)
    (fire player)
    (setf last-x x last-y y)
    (move player (+ direction (aref shift-dir shift-index)) 100)))

(defmethod die ((player player))
  (with-slots (x y shift-cooldowns reload-cooldown) player
    (loop for cd across shift-cooldowns do
	 (setf (cooldown-timer cd) 0.0))
    (setf (cooldown-timer reload-cooldown) 0.0)
    (call-next-method)
    (move-window-to (current-buffer) (- x (/ *width* 2)) (- y (/ *height* 2)))))

(defmethod update ((player player))
  (with-slots (direction
	       max-speed
	       last-x last-y
	       x y
	       color
	       width height			 
	       reload-cooldown
	       off-platform-cooldown
	       bullet-type
	       shift-cooldowns
	       stun-lock) player
    
    (setf direction (find-heading (+ x (/ width 2.0))
				  (+ y (/ height 2.0))
				  (screen-pointer-x) (screen-pointer-y))
	  last-x x last-y y)
    
    ;; holding up and the cursor is not in the player's bounding rectangle
    (if (and (holding-up-arrow)
	       (not (rect-in-rectangle-p
		     (cfloat (screen-pointer-x)) (cfloat (screen-pointer-y))
		     (cfloat 1) (cfloat 1)
		     (cfloat y) (cfloat x)
		     (cfloat width) (cfloat height)))
	       (>= (cooldown-timer stun-lock) (cooldown-time stun-lock)))
	(move player direction (* max-speed *dt*))
	(incf (cooldown-timer stun-lock) *dt*))
    
    ;; adjust the camera so that player remains in the center
    ;; (let ((cam-diff-x (- (+ (/ width 2.0) x) (slot-value (current-buffer) 'window-x)))
    ;; 	  (cam-diff-y (- (+ (/ width 2.0) y) (slot-value (current-buffer) 'window-y))))
    ;;   (move-window-to (current-buffer)
    ;; 		      (slot-value (current-buffer) 'window-x)
    ;; 		      (cond ((< (- (/ *screen-height* 2.0) cam-diff-y) (/ *screen-height* 15.0))
    ;; 			     (slot-value (current-buffer) 'window-y))
    ;; 			    ((> cam-nndiff-y (/ *screen-height* 2.0))
    ;; 			     (+ (slot-value (current-buffer) 'window-y) (* (/ max-speed 1.5) *dt*)))
    ;; 			    ((< cam-diff-y (/ *screen-height* 2.0))
    ;; 			     (- (slot-value (current-buffer) 'window-y) (* (/ max-speed 1.5) *dt*)))))
    ;;   (move-window-to (current-buffer)
    ;; 		      (cond ((< (- (/ *screen-height* 2.0) cam-diff-x) (/ *screen-height* 15.0))
    ;; 			     (slot-value (current-buffer) 'window-x))
    ;; 			    ((> cam-diff-x (/ *screen-height* 2.0))
    ;; 			     (+ (slot-value (current-buffer) 'window-x) (* (/ max-speed 1.5) *dt*)))
    ;; 			    ((< cam-diff-x (/ *screen-height* 2.0))
    ;; 			     (- (slot-value (current-buffer) 'window-x) (* (/ max-speed 1.5) *dt*))))
    ;; 		      (slot-value (current-buffer) 'window-y)))
    
    ;; left mouse firing
    (when (< (cooldown-timer reload-cooldown) (cooldown-time reload-cooldown))
      (incf (cooldown-timer reload-cooldown) *dt*))

    (loop for cd across shift-cooldowns do
       ;;(format t "~a~%" (cooldown-timer cd))
	 (when (< (cooldown-timer cd) (cooldown-time cd))
	   (incf (cooldown-timer cd) *dt*)))

    (cond ((pressed-left-arrow) (setf bullet-type (mod (+ bullet-type -1) 3)))
	  ((pressed-right-arrow) (setf bullet-type (mod (+ bullet-type 1) 3))))
    
    (cond ((and (> (cooldown-timer reload-cooldown) ;; left mouse
		   (cooldown-time reload-cooldown))
		(holding-left-mouse))
	   (fire player)
	   (setf (cooldown-timer reload-cooldown) 0.0))
	  ((and (> (cooldown-timer (aref shift-cooldowns 0)) ;; up-left
		   (cooldown-time (aref shift-cooldowns 0)))
		(holding-up-left-arrow))	   
	   (teleport player 0))
	  ((and (> (cooldown-timer (aref shift-cooldowns 1)) ;; up-right
		   (cooldown-time (aref shift-cooldowns 1)))
		(holding-down-left-arrow))
	   (teleport player 1))
	  ((and (> (cooldown-timer (aref shift-cooldowns 2)) ;; down-left
		   (cooldown-time (aref shift-cooldowns 2)))
		(holding-down-right-arrow))
	   (teleport player 2))
	  ((and (> (cooldown-timer (aref shift-cooldowns 3)) ;; down-right
		   (cooldown-time (aref shift-cooldowns 3)))
		(holding-up-right-arrow))
	   (teleport player 3)))
    (call-next-method)))

(defmethod draw ((player player))
  (with-slots (x y width height direction color		 
		 death-cooldown
		 death-reset-cooldown
		 death-reset-speed
		 bullet-type
		 reload-cooldown
		 shift-color
		 shift-cooldowns) player
    (draw-textured-rectangle-* x y 0 width height
			       (find-texture "up.png") ;;place holder image
			       :angle (radians->degrees (+ (/ pi 2) direction))
			       :tex-w (find-resource-property "up.png" :width)
			       :tex-h (find-resource-property "up.png" :height)
			       :clip-x 9 :clip-y 9 :clip-w 45 :clip-h 45
			       :vertex-color color)
    
    (when (> (cooldown-timer death-cooldown) 0.2)
      (let ((overlay-alpha (* 255 (/ (cooldown-timer death-cooldown)
				     (cooldown-time death-cooldown)))))	
	(draw-textured-rectangle 0 0 0 *width* *height*
				 (find-texture "square.png")
				 :vertex-color `(0.0 0.0 0.0 ,overlay-alpha))))
    
    ;; cooldown ui
    (let ((alphas (loop for cd across shift-cooldowns
		     collect (if (< (cooldown-timer cd) (cooldown-time cd))
				 (* 100.0 (/ (cooldown-timer cd) (cooldown-time cd)))
				 255.0)))
	  (reload-alpha (if (< (cooldown-timer reload-cooldown) (cooldown-time reload-cooldown))
			    (* 100.0
			       (/ (cooldown-timer reload-cooldown) (cooldown-time reload-cooldown)))
			    255.0))
	  (ui-size (units 1.7)))
      (flet ((get-cd-ui-color (index)
	       (enum-rgb (+ bullet-type (aref shift-color index)) (elt alphas index))))		
	(draw-box 0 (- *height* (* ui-size 2)) ui-size ui-size
		  :color (get-cd-ui-color 0))
	(draw-box 0 (- *height* ui-size) ui-size ui-size
		  :color (get-cd-ui-color 1))
	(draw-box ui-size (- *height* ui-size) ui-size ui-size
		  :color (get-cd-ui-color 2))
	(draw-box ui-size (- *height* (* ui-size 2)) ui-size ui-size
		  :color (get-cd-ui-color 3)))
      (draw-box (- *width* ui-size) (- *height* ui-size) ui-size ui-size
		:color (enum-rgb bullet-type reload-alpha)))))

(defmethod collide ((player player) (wall wall))
  (with-slots (direction max-speed last-x last-y x y width height) player
    ;;(format t "~a ~a~%" last-x last-y)
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

