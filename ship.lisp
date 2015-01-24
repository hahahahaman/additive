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
   (off-platform
    :initform nil)
   (off-platform-cooldown
    :initform (make-cooldown :time 2.0)
    :type cooldown)))

(defmethod collide ((ship ship) (stage stage))
  (with-slots (off-platform off-platform-cooldown) ship
    (setf (cooldown-timer off-platform-cooldown) 0.0
	  off-platform nil)))
