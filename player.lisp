;;;
;;; player.lisp
;;; 
;;; The player derived from a ship since it is a ship.
;;; File includes movement functions as well.


(in-package #:rgbshift)

(defclass player (ship)
  ((height :initform 16)
   (width :initform 16)
   (color :initform "white")
   (speed :initform 400)
   (speed-x :initform 10)
   (speed-y :initform 10)
   (direction :initform nil)
   (last-x :initform 0)
   (last-y :initform 0)
   (point-x :initform 0) ;;relative pointer positions
   (point-y :initform 0)))

;;; Keyboard press sensing functions

(defun holding-down-arrow ()
  (or (joystick-button-pressed-p :down)
      (keyboard-down-p :kp2)
      (keyboard-down-p :down)
      (keyboard-down-p :s)))

(defun holding-up-arrow()
  (or (joystick-button-pressed-p :up)
      (keyboard-down-p :kp8)
      (keyboard-down-p :up)
      (keyboard-down-p :w)))

(defun holding-left-arrow ()
  (or (joystick-button-pressed-p :left)
      (keyboard-down-p :kp4)
      (keyboard-down-p :left)
      (keyboard-down-p :a)))

(defun holding-right-arrow()
  (or (joystick-button-pressed-p :right)
      (keyboar-down-p :kp6)
      (keyboard-down-p :right)
      (keyboard-down-p :a)))

(defun rect-in-rectangle-p (x y width height o-top o-left o-width o-height)
  (declare (single-float x y width height o-top o-left o-width o-height)
	   (optimize (speed 3)))
  (not (or
	(<= (+ o-top o-height) y)
	(<= (+ y height) o-top)
	(<= (+ x width) o-left)
	(<= (+ o-left o-width) x))))

(defmethod update ((player player))
  (with-slots (direction speed
	       last-x last-y
	       x y
	       color
	       width height
	       point-x point-y) player
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
    (follow-with-camera (current-buffer) player)))
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

