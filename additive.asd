;;;; additive.asd

(asdf:defsystem #:additive
  :description "Asteroids meets smash meets idk some kind of minimalist thing."
  :author "ahaaahaa <hahahadude@gmail.com>"
  :license "Licenseless Rider"
  :depends-on (#:xelf)
  :serial t
  :components ((:file "package")
	       (:file "globals")
	       (:file "utils")	    
	       (:file "cooldown")
	       (:file "updater")
	       (:file "wall")	      
	       (:file "abyss")
	       (:file "stage")	       
	       (:file "ship")
	       (:file "bullet")
	       (:file "player")
	       (:file "basic-enemy")	     	      
	       (:file "level")
               (:file "additive")))

