(require 'sb-posix)

(load #P"~/quicklisp/setup.lisp")
(push (merge-pathnames "lib/" *default-pathname-defaults*)
      asdf:*central-registry*)

(ql:quickload '(:lispbuilder-sdl-mixer :lispbuilder-sdl-ttf :lispbuilder-sdl-image :uuid :cl-opengl :cl-fad ))
(asdf:load-system :xelf)
(asdf:load-system :additive)

(push #p"/home/demonseaman/quicklisp/local-projects/additive/" asdf:*central-registry*)

(asdf:oos 'asdf:load-op 'additive)
(sb-ext:save-lisp-and-die "additive.bin"
			  :toplevel (lambda ()
				      (sb-posix:putenv
				       (format nil "SBCL_HOME=~A" 
				      	       #.(sb-ext:posix-getenv "SBCL_HOME")))
				      (setf xelf:*executable* t)
				      ;;(setf xelf:*suppress-warnings* t)
				      (additive:additive)
				      0)
			  :executable t)

