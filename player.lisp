;;;
;;; player.lisp
;;; 
;;; The player derived from a ship since it is a ship.
;;; File includes movement functions as well.

(in-package #:rgbshift)

;;; Keyboard press sensing functions

(defclass player (ship)
  ((height
    :initform 16
    :type (unsigned-byte 8))
   (width
    :initform 16
    :type (unsigned-byte 8))
   (color
    :initform '(255 0 0 255)
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
   (point-x
    :initform 0.0
    :type short-float) ;;relative pointer positions
   (point-y
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
				   :direction direction)))
	(insert bullet)
	(move-to bullet x y))))
	;; (let ((pi/4 (/ pi 4))
	;;       (move-to bullet		
	;; 	(cond ((and (<= pi/4 direction)
	;; 		    (<= direction (* 3 pi/4)))
	;; 	       (- x 1))
	;; 	      ((and (<= (* 5 pi/4) direction)
	;; 		    (<= direction (* 7 pi/4)))
	;; 	       (+ x width 1))
	;; 	      (t (+ x (/ width 2.0) (* (/ width 2.0) (cos direction)))))
	;; 	(cond ((and (<= (- pi/4) direction)
	;; 		    (<= direction pi/4))
	;; 	       (- y 1))
	;; 	      ((and (<= (* 3 pi/4) direction)
	;; 		    (<= direction (* 5 pi/4)))
	;; 	       (+ y height 1))
	;; 	      (t (+ y (/ height 2.0) (* (/ height 2.0) (sin direction)))))))))))

(defmethod update ((player player))
  (with-slots (direction speed
			 last-x last-y
			 x y
			 color
			 width height
			 point-x point-y
			 reload-cooldown) player
    (setf point-x (+ (window-pointer-x) (slot-value (current-buffer) 'window-x))
	  point-y (+ (window-pointer-y) (slot-value (current-buffer) 'window-y))
	  direction (find-heading (+ x (/ width 2.0))
				  (+ y (/ height 2.0))
				  point-x point-y)
	  last-x x last-y y)
    
    ;; holding up and the cursor is not in the player's bounding rectangle
    (when (and (holding-up-arrow)
	       (not (rect-in-rectangle-p
		     (cfloat point-x) (cfloat point-y)
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
      (setf (cooldown-timer reload-cooldown) 0.0))))
;; (snap-window-to-node (current-buffer) player)))
;; (move-window (current-buffer) (- x last-x) (- y last-y)))))
;;(move-window-to-node (current-buffer) player))

(defmethod draw ((player player))
  (with-slots (x y width height point-x point-y direction) player
    (draw-textured-rectangle-* x y 0 width height
			       (find-texture "up") ;;place holder image
			       :angle (+ 90 (*
					     (/ 180.0 pi)
					     direction))
			       :tex-w (find-resource-property "up" :width)
			       :tex-h (find-resource-property "up" :height)
			       :clip-x 9 :clip-y 9 :clip-w 45 :clip-h 45
			       :vertex-color '(255 0 0 255))))

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
