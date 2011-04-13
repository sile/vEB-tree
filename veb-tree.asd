(require :asdf)

(defsystem veb-tree
  :name "veb-tree"
  :author "Takeru Ohta"
  :version "0.0.1"
  :description "van Emde Boas tree"
  :serial t 
  :components ((:file "package")
               (:file "veb-tree")))
