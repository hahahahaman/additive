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
