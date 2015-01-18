(in-package #:rgbshift)

(defclass level (buffer)
  ((updater :initform (make-instance 'updater))
   (player
    :initform (make-instance 'player)
    :accessor player)
   (background-color :initform "blue"
		     :accessor bg-color)
   (width :initform 10000)
   (height :initform 10000)))

(defmethod start-game ((level level))
  (with-slots (updater player) level
    (with-buffer level
      (insert player)
      (move-to player 320 240)
      (paste level (make-border 100 100 (- 1000 (units 1)) (- 1000 (units 1))))
      (insert updater))))
    
  
   
