;;;
;;; globals.lisp
;;;
;;; Dynamic scope variables and such.

(in-package #:additive)

(defparameter *width* 800)
(defparameter *height* 600)

(defparameter *dt* 0) ;; delta time

(defparameter *unit* 16)

(defresource (:name "square.png" :type :image :file "data/square.png" ;;:properties (:filter :linear)
		    ))
(defresource (:name "space.png" :type :image :file "data/space.png" ;;:properties (:filter :linear)
		    ))
(defresource (:name "dying-overlay.png" :type :image :file "data/dying-overlay.png"
		    ;;:properties (:filter :linear)
		    ))
(defresource (:name "beepr.wav" :type :sample :file "data/beepr.wav" :properties (:volume 10)))
(defresource (:name "beepg.wav" :type :sample :file "data/beepg.wav" :properties (:volume 10)))
(defresource (:name "beepb.wav" :type :sample :file "data/beepb.wav" :properties (:volume 10)))
(defresource (:name "song.ogg" :type :music :file "data/Edward my heart Beats 5 u.ogg"
	       :properties (:volume 200)))



