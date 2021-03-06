(in-package #:additive)

(defclass ship (node)
  ((height
    :initform 16
    :type (unsigned-byte 8))
   (width
    :initform 16
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
   (death-cooldown
    :initform (make-cooldown :time 5.0)
    :type cooldown)
   (death-reset-cooldown
    :initform (make-cooldown :time 1.0))
   (death-reset-speed
    :initform 2.0
    :type 'single-float)
   (knock-back-direction
    :initform nil)
   (knock-back
    :initform (make-cooldown :time 0.1 :timer 0.1)
    :type 'cooldown)
   (knock-back-speed
    :initform 1000)
   (stun-lock
    :initform (make-cooldown :time 0.1 :timer 0.1))))

(defgeneric fire (ship))
(defgeneric die (ship))

(defmethod die ((ship ship))
  ;; TODO: die
  (with-slots (x y width height) (slot-value (current-buffer) 'stage) 
    (move-to ship (+ x (/ width 2)) (+ y (/ height 2))))
  (with-slots (x
	       y
	       death-cooldown
	       death-reset-cooldown
	       color
	       last-x last-y
	       max-speed
	       knock-back
	       stun-lock) ship
    (setf (cooldown-timer death-reset-cooldown) 0.0
	  (cooldown-timer death-cooldown) 0.0
	  color '(0 0 0 255)
	  last-x x
	  last-y y
	  max-speed 400.0
	  knock-back (make-cooldown :time 0.1 :timer 0.1)
	  stun-lock (make-cooldown :time 0.1 :timer 0.1))))

(defmethod update ((ship ship))
  (with-slots (color
	       max-speed
	       direction
	       death-cooldown
	       death-reset-cooldown
	       death-reset-speed
	       knock-back-direction
	       knock-back
	       knock-back-speed
	       stun-lock) ship

    (when (< (cooldown-timer knock-back) (cooldown-time knock-back))
      (incf (cooldown-timer knock-back) *dt*)
      (move ship knock-back-direction (* knock-back-speed *dt*)))
    
    ;; when off stage 
    (when (not (colliding-with-p ship (slot-value (current-buffer) 'stage)))
      (setf (cooldown-timer death-reset-cooldown) 0.0) ;; reset the resetter
      (incf (cooldown-timer death-cooldown) *dt*)) ;; increase death timer
    
    (when (> (cooldown-timer death-cooldown) (cooldown-time death-cooldown))
      ;; you dead
      (die ship))))

(defmethod draw ((ship ship))
  t)

(defmethod collide ((ship ship) (stage stage))
  "The ship is on the stage."
  (with-slots (death-cooldown
	       death-reset-cooldown
	       death-reset-speed) ship
    
    ;; if the ship gets knocked off and comes back to stage
    ;; takes time for the reset period to start
    (when (> (cooldown-timer death-cooldown) 0.0)
      ;; if resetting is occuring
      (if (< (cooldown-timer death-reset-cooldown)
	     (cooldown-time death-reset-cooldown))	   
	  (incf (cooldown-timer death-reset-cooldown) *dt*) ;; closer to resetting
	  (progn ;; actually reset	      
	    (decf (cooldown-timer death-cooldown) (* death-reset-speed *dt*))
	    (when (<= (cooldown-timer death-cooldown) 0.0)
	      (setf (cooldown-timer death-cooldown) 0.0
		    (cooldown-timer death-reset-cooldown) 0.0)))))))

;; (defmethod collide ((ship ship) (abyss abyss))
;;   "The ship is off the stage."
;;   (with-slots (off-platform
;; 	       death-cooldown
;; 	       death-reset-cooldown
;; 	       death-reset-speed) ship
;;     (setf off-platform T)
;;     (when off-platform
;;       (print (cooldown-timer death-cooldown))
;;       (incf (cooldown-timer death-cooldown) *dt*))
;;     (when (> (cooldown-timer death-cooldown) (cooldown-time death-cooldown))
;;       ;; you dead
;;       (die ship))))
