;;;
;;; utils.lisp
;;;
;;; Useful macros and functions

(in-package #:additive)

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
  error. Remember to hit C in slime or pick the restart so errors don't kill the app."
  `(restart-case
       (progn ,@body) (continue () :report "Continue")))

;; (defun update-swank ()
;;   "Called from within the main loop, this keep the lisp repl running"
;;   (continuable
;;    (let ((connection (or swank::*emacs-connection* (swank::default-connection))))
;;      (when connection
;;        (swank::handle-requests connection t)))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))


(defun units (n)
  "Unit size for blocks."
  (* *unit* n))

(defun bullet-size () (units 0.5))

(defun screen-pointer-x ()
  "Pointer position relative to the current-buffer window-x."
  (+ (window-pointer-x) (slot-value (current-buffer) 'window-x)))

(defun screen-pointer-y ()
  "Pointer position relative to the current-buffer window-y."
  (+ (window-pointer-y) (slot-value (current-buffer) 'window-y)))

;;; Input sensing functions
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
      (keyboard-down-p :kp6)
      (keyboard-down-p :right)
      (keyboard-down-p :a)))

(defun pressed-left-arrow ()
  (keyboard-pressed-p :a))

(defun pressed-right-arrow ()
  (keyboard-pressed-p :d))

(defun pressed-down-arrow ()
  (keyboard-pressed-p :s))

(defun holding-up-left-arrow ()
  (keyboard-down-p :q))

(defun holding-up-right-arrow ()
  (keyboard-down-p :e))

(defun holding-down-left-arrow ()
  (keyboard-down-p :z))

(defun holding-down-right-arrow ()
  (keyboard-down-p :c))

(defun holding-left-mouse ()
  (sdl:mouse-left-p))

(defun holding-right-mouse ()
  (sdl:mouse-right-p))

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
  (multiple-value-bind (sx sy) (world-to-screen-coord (first args) (second args))
    (apply draw-func (cons sx (cons sy (nthcdr 2 args))))))

(defun enum-rgb (index &optional (alpha 255.0))
  (ecase (mod index 3)
    (0 `(255.0 0.0 0.0 ,alpha))
    (1 `(0.0 255.0 0.0 ,alpha))
    (2 `(0.0 0.0 255.0 ,alpha))))

(defun get-rgb-color-list (color)
  "Converts string colors to list form."
  (if (stringp color)
      (mapcar #'(lambda (n) (* n 255.0))
	      (gl-color-values-from-string color))
      color))

(defun add-rgb-color-list (a b)
  (let ((return-value '())
	(a-color-list (get-rgb-color-list a))
	(b-color-list (get-rgb-color-list b)))
    (loop for i from 0 to 2 do
	 (setf return-value (cons (+ (* (log (1+ (nth i b-color-list)) 10)
					(log (1+ (nth i a-color-list))10))
				     (* (log (1+ (nth i a-color-list)) 2)) (nth i b-color-list))
				  return-value)))
    (reverse return-value)))

(defun radians->degrees (radians)
  (* (/ 180 pi) radians))

(defun degrees->radians (degrees)
  (* (/ pi 180) degrees))

(defun rotate-point-ccw (x y radians)
  (values
   (+ (* x (cos radians)) (* y (- (sin radians))))
   (+ (* x (sin radians)) (* y (cos radians)))))
