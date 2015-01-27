;;;
;;; globals.lisp
;;;
;;; Dynamic scope variables and such.

(in-package #:rgbshift)

(defparameter *width* 800)
(defparameter *height* 600)

(defparameter *dt* 0) ;; delta time

(defparameter *unit* 16)

(defresource "data/square.png")
(defresource "data/space.png")
(defresource "data/dying-overlay.png")
