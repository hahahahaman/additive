Prototyping a top down fighting game kind of like sumo with teleportation and color shifting projectiles.

Some code to start up a devel session.
(progn (ql:quickload :xelf)
       (ql:quickload :rgbshift)
       (ql:quickload :plong)
       (ql:quickload :lispbuilder-sdl-examples))
       
(setf xelf:*user-projects-directory* #p"~/quicklisp/local-projects")
