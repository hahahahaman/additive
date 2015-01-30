;;;; additive.asd

(asdf:defsystem #:additive
  :description "Describe additive here"
  :author "Your Name <your.name@example.com>"
  :license "Licenseless Rider"
  :depends-on (#:xelf)
  :serial t
  :components ((:file "package")
	       (:file "utils")
	       (:file "globals")
	       (:file "cooldown")
	       (:file "updater")
	       (:file "wall")
	       (:file "abyss")
	       (:file "stage")
	       (:file "bullet")
	       (:file "ship")
	       (:file "player")
	       (:file "level")
               (:file "additive")))

