(defpackage veb-tree
  (:use :common-lisp)
  (:shadow :common-lisp push pop remove find copy-tree)
  (:export tree
           tree-p
           make
           push
           pop
           head
           remove
           find
           next
           empty-p
           size
           each
           from-list
           to-list
           
           node))
(in-package :veb-tree)
