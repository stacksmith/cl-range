(asdf:defsystem #:cl-range
  :description "A tree of objects that occupy a certain amount of width"
  :author "stacksmith"
  :license "bsd"
  :serial t
  :depends-on (#:cl-doubletree
	       )
  :components ((:file "package")
               (:file "range")))

