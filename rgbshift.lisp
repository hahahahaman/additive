;;;; rgbshift.lisp

(in-package #:rgbshift)

;;; Hacks and glory await!

(defun set-screen-dim (w h)
  (setf *width* w)
  (setf *height* h)
  (setf *screen-height* h)
  (setf *screen-width* w)
  (setf *gl-screen-width* w)
  (setf *gl-screen-height* h))

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
      ;;(follow-with-camera level (slot-value level 'player))
      (switch-to-buffer level)
      (start-game level))))

    
