Prototyping a top down fighting game kind of like sumo with teleportation and color shifting projectiles.
Built with xelf.

If you want to build go to https://github.com/hahahahaman/xelf follow the INSTALL file
This a slightly modified fork of https://github.com/dto/xelf

Linux:
Get sdl, sdl-mixer, sdl-image, sdl-gfx, and sdl-ttf.

Devel:
Some code to start up a devel session.
(progn (ql:quickload :xelf)
       (ql:quickload :additive)
       (ql:quickload :plong)
       (ql:quickload :lispbuilder-sdl-examples))
       
(setf xelf:*user-projects-directory* #p"~/quicklisp/local-projects")
