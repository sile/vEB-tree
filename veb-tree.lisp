(in-package :veb-tree)
;; see: http://en.wikipedia.org/wiki/Van_Emde_Boas_tree

(defstruct node
  bit-length
  children
  min
  max
  aux)

(defun own-bit-length (node)
  (ceiling (node-bit-length node) 2))

(defun descendants-bit-length (node)
  (floor (node-bit-length node) 2))

(defun children-count (node)
  (expt 2 (own-bit-length node)))

(defun child-max-element-count (node)
  (expt 2 (descendants-bit-length node)))

(defun make (max-element-count)
  (let ((bit-len (integer-length max-element-count)))
    (make-node :bit-length bit-len
               :min most-positive-fixnum
               :max -1)))

(defun empty-p (node)
  (with-slots (min max) node
    (> min max)))

(defun has-children-p (node)
  (not (null (node-children node))))

(defun ensure-node-children (node)
  (with-slots (children bit-length) node
    (when (null children)
      (setf children
            (loop WITH child-max-elem = (child-max-element-count node)
                  REPEAT (children-count node)
                  COLLECT (make child-max-elem) INTO list
                  FINALLY (return (coerce list 'vector)))))
    children))

(defun ensure-node-aux (node)
  (with-slots (aux) node
    (when (null aux)
      (setf aux (make (children-count node))))
    aux))

(defun get-child (elem node &key ensure)
  (when ensure
    (ensure-node-children node))

  (let ((index (ldb (byte (own-bit-length node)
                          (descendants-bit-length node))
                    elem)))
    (values (aref (node-children node) index) 
            index)))

(defun push-impl (x node)
  (with-slots (min max) node
    (cond ((empty-p node)
           (setf min x 
                 max x))
          ((= min max)
           (if (< x min)
               (setf min x)
             (setf max x)))
          (t 
           (when (< x min) (rotatef x min))
           (when (> x max) (rotatef x max))
           
           (multiple-value-bind (child index-of-child)
                                (get-child x node :ensure t)
             (when (empty-p child)
               (push-impl index-of-child (ensure-node-aux node)))
             (push-impl x child))))))

(defun push (elem tree)
  (push-impl elem tree)
  tree)

(defun find-impl (x node)
  (with-slots (min max) node
    (cond ((not (<= min x max)) nil)
          ((= x min) t)
          ((= x max) t)
          ((not (has-children-p node)) nil)
          (t
           (find-impl x (get-child x node))))))
           
(defun find (elem tree)
  (find-impl elem tree))

(defun next-impl (x node)
  (with-slots (min max children aux) node
    (cond ((<= x min) min)
          ((>  x max) nil)
          ((not (has-children-p node)) max)
          (t
           (multiple-value-bind (child index-of-child) (get-child x node)
             (if (<= x (node-max child))
                 (next-impl x child)
               (let ((nearest-valid-index (next-impl (1+ index-of-child) aux)))
                 (if (null nearest-valid-index)
                     max
                   (node-min (aref children nearest-valid-index))))))))))
           
(defun next (elem tree)
  (next-impl elem tree))

(defun remove-impl (x node)
  (with-slots (min max aux children) node
    (cond ((= x min max)
           (setf min most-positive-fixnum
                 max -1))
          ((not (has-children-p node))
           (cond ((= x min) (setf min max))
                 ((= x max) (setf max min))
                 (t         nil)))
          (t
           (when (= x min)
             (setf x (node-min (aref children (node-min aux)))
                   min x))
           (when (= x max)
             (setf x (node-max (aref children (node-max aux)))
                   max x))
           
           (multiple-value-bind (child index-of-child) (get-child x node)
             (remove-impl x child)
             (when (empty-p child)
               (remove-impl index-of-child aux)
               (when (empty-p aux)
                 (setf children nil
                       aux nil))))))))

(defun remove (elem tree)
  (remove-impl elem tree))
