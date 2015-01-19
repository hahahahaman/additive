(in-package #:rgbshift)

(defclass level (buffer)
  ((updater
    :initform (make-instance 'updater)
    :type updater)
   (player
    :initform (make-instance 'player)
    :type player
    :accessor player)
   ;;(background-color :initform "blue")
   ;;(background-image :initform "data/space.png")
   (width :initform 3000)
   (height :initform 3000)
   (window-scrolling-speed :initform 10)))

(defmethod draw  ((level level))
  (with-slots (width height) level
    (draw-textured-rectangle-* 0 0 0 width height (find-texture "data/space.png")))
  (call-next-method))

(defmethod start-game ((level level))
  (with-slots (updater player window-x window-y) level
    (with-buffer level
      (insert player)
      (move-to player 1320 1240)
      (paste level (make-border 1000 1000 (- 1000 (units 1)) (- 1000 (units 1))))
      (insert updater)
      (setf window-x 800 window-y 800))))
    
  
   
