;;;
;;; globals.lisp
;;;
;;; Dynamic scope variables and such.

(in-package #:additive)

(defparameter *width* 800)
(defparameter *height* 600)

(defparameter *dt* 0) ;; delta time

(defparameter *unit* 16)

(defresource (:name "square.png" :type :image :file "data/square.png"
		    ))
(defresource (:name "up.png" :type :image :file "data/up.png" ;;:properties (:filter :linear)
		    ))
(defresource (:name "beepr.wav" :type :sample :file "data/beepr.wav" :properties (:volume 10)))
(defresource (:name "beepg.wav" :type :sample :file "data/beepg.wav" :properties (:volume 10)))
(defresource (:name "beepb.wav" :type :sample :file "data/beepb.wav" :properties (:volume 10)))
(defresource (:name "song.ogg" :type :music :file "data/Edward my heart Beats 5 u.ogg"
	       :properties (:volume 200)))

(defresource (:name "sans" :type :ttf :file "data/DejaVuSans.ttf" :properties (:size 11)))

