(in-package #:rgbshift)

(defclass level (buffer)
  ((updater :initform (make-instance 'updater))
   (player
    :initform (make-instance 'player))
   (background-color :initform "blue"
		     :accessor bg-color)
   (width :initform 1000)
   (height :initform 1000)))

(defmethod start-game ((level level))
  (with-slots (updater player) level
    (with-buffer level
      (insert player)
      (move-to player 320 240)
      (paste level (make-border 0 0 (- *width* (units 1)) (- *height* (units 1))))
      (insert updater))))
    
  
   
