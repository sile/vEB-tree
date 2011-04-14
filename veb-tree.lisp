(in-package :veb-tree)
;; see: http://en.wikipedia.org/wiki/Van_Emde_Boas_tree

(defstruct veb-node 
  (min most-positive-fixnum)
  (max most-negative-fixnum)
  aux
  children
  element-count-limit)

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

(defun find (elem tree)
  (labels ((recur (x node)
             (with-slots (min max children) node
               (when (not (<= min x max))
                 (return-from recur nil))

               (when (= x min)
                 (return-from recur t))

               (when (= x max)
                 (return-from recur t))
               
               (when children
                 (let* ((child-count (length children))
                        (i (floor x child-count))
                        (child (aref children i)))
                   (recur (mod x child-count) child))))))
    (recur elem tree)))

(defun find-next (elem tree)
  (labels ((recur (x node)
             (print (list :in x))
             (with-slots (min max children element-count-limit) node
               (print (list :range min max))
               (when (<= x min)
                 (return-from recur min))

               (when (> x max)
                 (return-from recur (1- element-count-limit)))

               (when (null children)
                 (return-from recur max))
               (print (list (ceiling (sqrt element-count-limit))
                            (length children)))

               (let* ((child-count (ceiling (sqrt element-count-limit))) ;;(length children))
                      (i (floor x child-count))
                      (lo-bits (mod x child-count))
                      (hi-bits (- x lo-bits))
                      (child (aref children i)))
                 (if (<= lo-bits (veb-node-max child))
                     (+ hi-bits (* child-count i) (recur lo-bits child)) ; =?= (veb-node-min child)
                   ;; XXX: if aux == nil, return max-value
                   (progn 
                     (print (list :aux (veb-node-aux node)))
                     (let ((ret (recur (1+ i) (veb-node-aux node))))
                       (print (list :ret ret
                                    :x x
                                    :i i
                                    :lo lo-bits
                                    :hi hi-bits
                                    :ch (aref children ret)))
                       (if (not (empty-p (aref children ret)))
                           (+ hi-bits 
                              (* child-count (1+ i))
                              (veb-node-min
                               (aref children ret)))
                         max))))))))
    (recur elem tree)))

#|
(defparameter *n* (veb-tree:push 15 (veb-tree:push 90 (veb-tree:push 3 (veb-tree:push 2 (veb-tree:push 10 (veb-tree:make 100)))))))
|#