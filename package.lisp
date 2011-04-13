(defpackage veb-tree
  (:use :common-lisp)
  (:shadow :common-lisp push pop remove find)
  (:export veb-tree
           veb-tree-p
           make
           push
           pop
           head
           remove
           find
           empty-p
           size
           each
           from-list))
(in-package :veb-tree)
