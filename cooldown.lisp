;;;
;;; cooldown.lisp
;;;
;;; Timer and cooldown time

(in-package #:additive)

(defstruct cooldown
  "Stores the time before cooldown is refreshed, cooldown-time,
and the timer count the time elapsed."
  (time 1.0 :type short-float)
  (timer 0.0 :type short-float))


