;;;; additive.lisp

(in-package #:additive)

;;; Hacks and glory await!

(defparameter *width* 1280)
(defparameter *height* 768)

(defun set-screen-dim (w h)
  (setf *width* w)
  (setf *height* h)
  (setf xelf:*screen-height* h)
  (setf xelf:*screen-width* w)
  (setf xelf:*gl-screen-width* w)
  (setf xelf:*gl-screen-height* h))

(eval-when (:load-toplevel) 
  (setf *current-directory*
	(make-pathname
	 :directory (pathname-directory #.#P"./"))))

;; the game
(defun additive ()
  (set-screen-dim 800 600)

  (setf *resizable* t)
  (setf *scale-output-to-window* t)
  (setf *fullscreen* nil)
  (setf *frame-rate* 60)
  (setf *window-title* "additive color")

  (with-session
    (open-project :additive)
    (index-all-images)
    (index-all-samples)
    (index-pending-resources)
    ;;(preload-resources)
    (let ((level (make-instance 'level)))
      (switch-to-buffer level)
      (follow-with-camera level (slot-value level 'player))
      (start-game level)
      (play-music "song.ogg" :loop t))))

    
