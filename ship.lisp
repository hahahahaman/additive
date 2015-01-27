(in-package #:rgbshift)

(defclass ship (node)
  ((height
    :initform 16
    :type (unsigned-byte 8))
   (width
    :initform 16
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
   (death-cooldown
    :initform (make-cooldown :time 2.0)
    :type cooldown)
   (death-reset-cooldown
    :initform (make-cooldown :time 1.0))
   (death-reset-speed
    :initform 2.0
    :type 'single-float)))

(defgeneric die (ship))

(defmethod die ((ship ship))
  ;; TODO: die
  (move-to ship 1320 1240))

(defmethod update ((ship ship))
  (with-slots (off-platform
	       death-cooldown
	       death-reset-cooldown
	       death-reset-speed) ship
    ;; when off stage
    (when (not (colliding-with-p ship (slot-value (current-buffer) 'stage)))
      (setf (cooldown-timer death-reset-cooldown) 0.0) ;; reset the resetter
      (incf (cooldown-timer death-cooldown) *dt*)) ;; increase death timer
    
    (when (> (cooldown-timer death-cooldown) (cooldown-time death-cooldown))
      ;; you dead
      (die ship))))

(defmethod collide ((ship ship) (stage stage))
  "The ship is on the stage."
  (with-slots (off-platform
	       death-cooldown
	       death-reset-cooldown
	       death-reset-speed) ship
    (with-slots (x y height width) stage
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
		      (cooldown-timer death-reset-cooldown) 0.0))))))))

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
