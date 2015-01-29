;;;
;;; wall.lisp
;;; 
;;; Rectangle that prevents movement

(in-package #:rgbshift)

(defclass wall (node)
  ((color
    :initform "black"
    :accessor wall-color)))

(defmethod draw ((wall wall))
  (with-slots (x y width height color) wall
    (draw-textured-rectangle-* x y 0  width height (find-texture "data/square.png")
			       :vertex-color color)))
     ;;(draw-image "splash.png" :width width :height height)))

(defun make-wall (x y width height)
  "Create and return wall instance."
  (let ((wall (make-instance 'wall)))
    (resize wall width height)
    (move-to wall x y)
    wall))

(defun make-border (x y width height)
  "Create a rectangular enclosing."
  (let ((left x)
	(top y)
	(right (+ x width))
	(bottom (+ y height)))
    (with-new-buffer
      ;; top
      (insert (make-wall left top (- right left) (units 1)))

      ;; bot
      (insert (make-wall left bottom (- right left (units -1)) (units 1)))

      ;; left
      (insert (make-wall left top (units 1) (- bottom top)))

      ;;right
      (insert (make-wall right top (units 1) (- bottom top (units -1))))

      ;;send it all back
      (current-buffer))))


    
