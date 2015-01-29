(in-package #:rgbshift)

(defclass level (buffer)
  ((width
    :initform 3000.0)
   (height
    :initform 3000.0)
   (updater
    :initform (make-instance 'updater)
    :type updater)
   (player
    :initform (make-instance 'player)
    :type player
    :reader player)
   (abyss
    :initform (make-instance 'abyss)
    :type abyss)
   (stage
    :initform (make-instance 'stage)
    :type stage)))

(defmethod draw  ((level level))
  (call-next-method))

(defmethod start-game ((level level))
  (with-slots (width height updater player abyss stage window-x window-y) level
    (with-buffer level
      (setf (slot-value abyss 'width) width
	    (slot-value abyss 'width) height)
      (insert abyss)
      (insert stage)
      (move-to stage 1000 1000)
      (insert player)
      (move-to player 1320 1240)
      (insert (make-wall 1000 1000 40 40))
      ;;(paste level (make-border 1000 1000 (- 1000 (units 1)) (- 1000 (units 1))))
      (insert updater)
      (setf window-x 800 window-y 800))))
