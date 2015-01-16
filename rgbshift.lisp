;;;; rgbshift.lisp

(in-package #:rgbshift)

;;; Hacks and glory await!

(defparameter *width* 640)
(defparameter *height* 480)

(defun set-screen-dim (w h)
  (setf *width* w)
  (setf *height* h)
  (setf xelf:*screen-height* h)
  (setf xelf:*screen-width* w)
  (setf xelf:*gl-screen-width* w)
  (setf xelf:*gl-screen-height* h))

;; the game
(defun rgbshift ()
  (set-screen-dim *width* *height*)

  (setf *resizable* t)
  (setf *scale-output-to-window* t)
  (setf *fullscreen* nil)
  (setf *frame-rate* 60)

  (with-session
    (open-project :rgbshift)
    (index-pending-resources)
    (let ((level (make-instance 'level)))
      (switch-to-buffer level)
      (start-game level))))

    
