;;;; rgbshift.asd

(asdf:defsystem #:rgbshift
  :description "Describe rgbshift here"
  :author "Your Name <your.name@example.com>"
  :license "Licenseless Rider"
  :depends-on (#:xelf)
  :serial t
  :components ((:file "package")
	       (:file "utils")
	       (:file "globals")	       
	       (:file "updater")
	       (:file "wall")
	       (:file "bullet")
	       (:file "ship")
	       (:file "player")
	       (:file "level")
               (:file "rgbshift")))

