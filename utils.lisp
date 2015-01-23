;;;
;;; utils.lisp
;;;
;;; Useful macros and functions

(in-package #:rgbshift)

(defmacro continuable (&body body)
  ;;"Helper macro that we can use to allow us to continue from an
  ;;error. Remember to hit C in slime or pick the restart so errors don't kill the app."
  `(restart-case
       (progn ,@body) (continue () :report "Continue")))

(defun update-swank ()
  ;;"Called from within the main loop, this keep the lisp repl
  ;;working while cepl runs"
  (continuable
   (let ((connection (or swank::*emacs-connection* (swank::default-connection))))
     (when connection
       (swank::handle-requests connection t)))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

;;; unit size for blocks
(defun units (n) (* *unit* n))

;;; keyboard press sensing functions
(defun holding-down-arrow ()
  (or (joystick-button-pressed-p :down)
      (keyboard-down-p :kp2)
      (keyboard-down-p :down)
      (keyboard-down-p :s)))

(defun holding-up-arrow()
  (or (joystick-button-pressed-p :up)
      (keyboard-down-p :kp8)
      (keyboard-down-p :up)
      (keyboard-down-p :w)))

(defun holding-left-arrow ()
  (or (joystick-button-pressed-p :left)
      (keyboard-down-p :kp4)
      (keyboard-down-p :left)
      (keyboard-down-p :a)))

(defun holding-right-arrow()
  (or (joystick-button-pressed-p :right)
      (keyboar-down-p :kp6)
      (keyboard-down-p :right)
      (keyboard-down-p :a)))

(defun left-mouse-pressed ()
  (sdl:mouse-left-p))

;;; 
(defun rect-in-rectangle-p (x y width height o-top o-left o-width o-height)
  "Nice to have for on the spot rectangle coliision."
  (declare (single-float x y width height o-top o-left o-width o-height)
	   (optimize (speed 3)))
  (not (or
	(<= (+ o-top o-height) y)
	(<= (+ y height) o-top)
	(<= (+ x width) o-left)
	(<= (+ o-left o-width) x))))

(defun world-to-screen-coord (x y)
  "Converts world coordinates to screen coordinates.
All drawing calls in xelf except for draw-textured-rectangle-*
seem to draw in position relative to the screen rather than the world."
  (with-slots (window-x window-y) (current-buffer)
    (values (- x window-x) (- y window-y))))

(defun draw-world-to-screen (draw-func &rest args)
  (multiple-value-bind (sx sy) (world-to-screen-coord (first args) (first args))
    (apply draw-func (cons sx (cons sy (nthcdr 2 args))))))
