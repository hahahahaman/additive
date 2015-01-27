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
    :initform '(0 0 0 255)
    :type list)
   (speed
    :initform 400.0
    :type short-float)
   (direction
    :initform 0.0
    :type short-float)
   (last-x
    :initform 0.0
    :type short-float)
   (last-y
    :initform 0.0
    :type short-float)
   (reload-cooldown
    :initform (make-cooldown :time 0.55)
    :type cooldown)))

(defgeneric fire (player))

(defmethod fire ((player player))
  (with-slots (direction x y width height) player
      (let ((bullet (make-instance 'bullet
				   :color '(255 0 0 255)
				   :direction direction))
	    (angle (+ (/ pi 2) direction))
	    (center-x (+ x (/ width 2)))
	    (center-y (+ y (/ height 2)))
	    (sqrt2 1.4142135))
	(insert bullet)

	;; Clockwise 2D lin. transformation
	;; and scaled so that bullet isn't in player hitbox
	(move-to bullet
		 (+ center-x (* sqrt2 (1+ (/ height -2)) (- (sin angle))))
		 (+ center-y (* sqrt2 (1+ (/ height -2)) (cos angle)))))))

(defmethod update ((player player))
  (with-slots (direction speed
			 last-x last-y
			 x y
			 color
			 width height			 
			 reload-cooldown
			 off-platform-cooldown) player
    
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
      (move player direction (* speed *dt*)))

    ;; adjust the camera so that player remains in the center
    ;; (let ((cam-diff-x (- (+ (/ width 2.0) x) (slot-value (current-buffer) 'window-x)))
    ;; 	  (cam-diff-y (- (+ (/ width 2.0) y) (slot-value (current-buffer) 'window-y))))
    ;;   (move-window-to (current-buffer)
    ;; 		      (slot-value (current-buffer) 'window-x)
    ;; 		      (cond ((< (- (/ *screen-height* 2.0) cam-diff-y) (/ *screen-height* 15.0))
    ;; 			     (slot-value (current-buffer) 'window-y))
    ;; 			    ((> cam-diff-y (/ *screen-height* 2.0))
    ;; 			     (+ (slot-value (current-buffer) 'window-y) (* (/ speed 1.5) *dt*)))
    ;; 			    ((< cam-diff-y (/ *screen-height* 2.0))
    ;; 			     (- (slot-value (current-buffer) 'window-y) (* (/ speed 1.5) *dt*)))))
    ;;   (move-window-to (current-buffer)
    ;; 		      (cond ((< (- (/ *screen-height* 2.0) cam-diff-x) (/ *screen-height* 15.0))
    ;; 			     (slot-value (current-buffer) 'window-x))
    ;; 			    ((> cam-diff-x (/ *screen-height* 2.0))
    ;; 			     (+ (slot-value (current-buffer) 'window-x) (* (/ speed 1.5) *dt*)))
    ;; 			    ((< cam-diff-x (/ *screen-height* 2.0))
    ;; 			     (- (slot-value (current-buffer) 'window-x) (* (/ speed 1.5) *dt*))))
    ;; 		      (slot-value (current-buffer) 'window-y)))
    
    ;; left mouse firing
    (when (< (cooldown-timer reload-cooldown) (cooldown-time reload-cooldown))
      (incf (cooldown-timer reload-cooldown) *dt*))
    
    (when (and (left-mouse-pressed)
	       (> (cooldown-timer reload-cooldown) (cooldown-time reload-cooldown)))
      (fire player)
      (setf (cooldown-timer reload-cooldown) 0.0))
    (call-next-method)))

(defmethod draw ((player player))
  (with-slots (x y width height direction color
	       death-cooldown
	       death-reset-cooldown
	       death-reset-speed) player
    (draw-textured-rectangle-* x y 0 width height
			       (find-texture "up") ;;place holder image
			       :angle (+ 90 (*
					     (/ 180.0 pi)
					     direction))
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
  (with-slots (direction speed last-x last-y x y width height) player
    ;;x movement pass
    (when (colliding-with-bounding-box-p 
	   wall
	   last-y x
	   (+ x width) (+ last-y height))
      (move-to player last-x y))

    ;;y movement pass
    (when (colliding-with-bounding-box-p
	   wall
	   y last-x
	   (+ last-x width) (+ y height))
      (move-to player x last-y))))

(defmethod collide ((player player) (stage stage))
  (call-next-method))
