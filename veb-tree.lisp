(in-package :veb-tree)

(defstruct node
  bit-length
  children
  min
  max
  aux)

(defstruct tree
  root
  size)

(defun own-bit-length (node)
  (ceiling (node-bit-length node) 2))

(defun descendants-bit-length (node)
  (floor (node-bit-length node) 2))

(defun children-count (node)
  (expt 2 (own-bit-length node)))

(defun child-max-element-count (node)
  (expt 2 (descendants-bit-length node)))

(defun %make-node (max-element-count)
  (let ((bit-len (integer-length max-element-count)))
    (make-node :bit-length bit-len
               :min most-positive-fixnum
               :max -1)))

(defun make (capacity)
  (make-tree :root (%make-node capacity) :size 0))

(defun size (tree)
  (tree-size tree))

(defun empty-p (tree)
  (node-empty-p (tree-root tree)))

(defun node-empty-p (node)
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
                  COLLECT (%make-node child-max-elem) INTO list
                  FINALLY (return (coerce list 'vector)))))
    children))

(defun ensure-node-aux (node)
  (with-slots (aux) node
    (when (null aux)
      (setf aux (%make-node (children-count node))))
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
    (cond ((node-empty-p node)
           (setf min x 
                 max x))
          ((or (= x min) (= x max))
           nil)
          ((= min max)
           (if (< x min)
               (setf min x)
             (setf max x)))
          (t 
           (when (< x min) (rotatef x min))
           (when (> x max) (rotatef x max))
           
           (multiple-value-bind (child index-of-child)
                                (get-child x node :ensure t)
             (when (node-empty-p child)
               (push-impl index-of-child (ensure-node-aux node)))
             (push-impl x child))))))

(defun push (elem tree)
  (when (push-impl elem (tree-root tree))
    (incf (tree-size tree)))
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
  (find-impl elem (tree-root tree)))

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
  (next-impl elem (tree-root tree)))

(defun remove-impl (x node)
  (with-slots (min max aux children) node
    (cond ((= x min max)
           (setf min most-positive-fixnum
                 max -1))
          ((not (has-children-p node))
           (cond ((= x min) (setf min max))
                 ((= x max) (setf max min))
                 (t nil)))  ; not exists
          (t
           (when (= x min)
             (setf min (setf x (node-min (aref children (node-min aux))))))
           (when (= x max)
             (setf max (setf x (node-max (aref children (node-max aux))))))
           
           (multiple-value-bind (child index-of-child) (get-child x node)
             (prog1 (remove-impl x child)
               (when (node-empty-p child)
                 (remove-impl index-of-child aux)
                 (when (node-empty-p aux)
                   (setf children nil
                         aux nil)))))))))

(defun remove (elem tree)
  (when (remove-impl elem (tree-root tree))
    (decf (tree-size tree)))
  tree)

(defun head (tree)
  (with-slots (root) tree
    (when (/= (node-min root) most-positive-fixnum)
      (node-min root))))

(defun pop (tree)
  (prog1 (head tree)
    (unless (empty-p tree)
      (remove (next 0 tree) tree)
      (decf (tree-size tree)))))

(defmacro each ((var tree &optional result-form) &body body)
  (let ((cur (gensym))
        (tree-val (gensym)))
    `(let ((,tree-val ,tree))
       (do* ((,cur (next 0 ,tree-val) (next (1+ ,cur) ,tree-val))
             (,var ,cur ,cur))
            ((null ,cur) ,result-form)
            ,@body))))

(defun to-list (tree &aux acc)
  (each (x tree (nreverse acc))
    (common-lisp:push x acc)))

(defun from-list (capacity list)
  (loop WITH tree = (make capacity)
        FOR x IN list
    DO
    (push x tree)
    FINALLY
    (return tree)))
