(in-package :veb-tree)
;; see: http://en.wikipedia.org/wiki/Van_Emde_Boas_tree

(defstruct veb-node 
  (min most-positive-fixnum)
  (max most-negative-fixnum)
  aux
  children
  element-count-limit)

(defstruct veb-tree
  root
  key-limit
  
  )

(defun find-next (tree x)
  (with-slots (root key-limit) tree
    (labels ((recur (node x)
               (with-slots (min max aux children) node
                 (cond ((<= x min) min)
                       ((> x max) key-limit)
                       (t
                        (let* ((i (floor (/ x (sqrt key-limit))))
                               (lo (mod x (sqrt key-limit)))
                               (hi (- x lo)))
                          (if (<= lo (veb-node-max (aref children i)))
                              (+ hi (recur (aref children i) lo))
                            (+ hi (veb-node-min
                                   (aref children (recur aux (1+ i))))))))))))
      (recur root x))))

(defun make (limit)
  (make-veb-node :element-count-limit limit))

(defun empty-p (node)
  (with-slots (min max) node
    (> min max)))

(defun min=max-p (node)
    (with-slots (min max) node
      (= min max)))

(defun push (elem tree)
  (labels ((ensure-children (node)
             (with-slots (children element-count-limit) node
               (when (null children)
                 (setf children
                       (loop REPEAT #1=(ceiling (sqrt element-count-limit))
                             COLLECT (make #1#) INTO list
                             FINALLY (return (coerce list 'vector)))))))
           (ensure-aux (node)
             (with-slots (aux element-count-limit) node
               (when (null aux)
                 (setf aux (make #1#)))))
           (recur (x node)
             (with-slots (min max children) node
               (when (> min max)
                 (setf min x max x)
                 (return-from recur))

               (when (= min max)
                 (if (< x min)
                     (setf min x)
                   (setf max x))
                 (return-from recur)) 

               (when (< x min)
                 (rotatef x min))
               (when (> x max)
                 (rotatef x max))
               
               (ensure-children node)
               (let* ((child-count (length children))
                      (i (floor x child-count))
                      (child (aref children i)))
                 (recur (mod x child-count) child)
                 (when (min=max-p child)
                   (ensure-aux node)
                   (push i (veb-node-aux node)))))))
    (recur elem tree))
  tree)
