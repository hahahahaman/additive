;;;
;;; updater.lisp
;;;
;;; Invisible utility object to be added to the objects of a buffer
;;; so that it can be updated during the buffers update function.
;;; Useful for fps counting and possibly debug drawing.

(in-package #:rgbshift)

(defclass updater (node)
  ((height :initform 1) ;; these should be set for drawing
   (width :initform 1) ;; so that things are actually shown
   (color :initform "blue")
   (fps-update :initform (sdl:sdl-get-ticks))
   (prev-num-updates :initform *updates*)
   (fps :initform *frame-rate*)))

(defmethod update :after ((updater updater))
  (when (keyboard-down-p :escape) (quit t)) ;; leave game

  ;;fps counter
  (with-slots (fps-update fps prev-num-updates) updater
    ;;after 1000 milisecs have passed since last fps update
    (when (>= (- (sdl:sdl-get-ticks) fps-update) 1000)
      (setf fps (- *updates* prev-num-updates)
	    fps-update (sdl:sdl-get-ticks)
	    *dt* (/ 1.0 fps)
	    prev-num-updates *updates*)))

  (with-slots (player) (current-buffer)
    (move-to updater (slot-value player 'x) (slot-value player 'y)))

  (update-swank)) ;;update dat swank server

(defmethod draw ((updater updater))
  (let ((fps-string (concatenate 'string "FPS: "
				 (write-to-string (slot-value updater 'fps)))))
    (draw-string fps-string 0 0))
  ;;(draw-image "data/space.png" 0  0 :height 1080 :width 1920)
  )
						  
  
  
