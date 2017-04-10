;; 
(in-package :cl-range)

;; A range is a datastructure designed to maintain a structured view into a
;; flat text buffer.  It consists of a hierarchy of 'range' elements which
;; nest but do not overlap.
;;
;; Each range identifies a Lisp object that is responsible for it, or nil
;; object.  When a sub-range is inserted into a range, it must create a nil
;; subrange of the same size first, and then subdivide it as needed. This
;; ensures that there are no 'gaps' to deal with separately.
;;
;; Ranges store their width; all subranges of a range add up to the same
;; width as their parent.
;;
;; Ranges are connected together as a double-linked-list to allow easy
;; bidirectional walking.  Each range has a dad and sub link to form
;; a tree.
;;
;; Note that ranges reflect 'positions' in the text buffer.
(define-condition range-error (simple-error)
  ((range :reader range-error-range
	  :initarg :range :initform nil)
   (msg   :reader range-error-msg
	  :initarg :msg :initform ""))
  (:report (lambda (condition stream)
             (format stream "Error on Range ~A: ~A"
                     (range-error-range condition)
                     (range-error-msg   condition)))))

(defclass range (tree:node)
  ((obj   :accessor obj :initarg :obj :initform nil)
   (width :accessor width :initarg :width :type fixnum :initform 0)))

(defun print-object-range (obj out)
  ;;(print-unreadable-object (obj out :type t))
  (format out "range: w:~A ~A " (width obj) (obj obj) )
     )
(defmethod print-object ((obj range) out)
 (print-object-range obj out))

;;==============================================================================
;;
;;------------------------------------------------------------------------------
(defun narrow (range &key (by 1))
  (unless (zerop by)
    (tree:for-up (lambda (range)
		 (unless (plusp (decf (width range) by))
		   (tree:unlink range)))
	       range)))
 
(defun widen (range &key (by 1))
  (declare (type fixnum by))
;;  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (zerop by)
    (tree:for-up (lambda (range)
		 (incf (width range) by))
	       range)
))
;;------------------------------------------------------------------------------
;; 
(defun range-widths-l (range)
  "Add up widths of range and all ranges to the left"
  (if (tree:terminal range)
      0 ;do not count the terminal!
      (+ (width range)
	 (range-widths-l (tree:prev range)))))

(defun range-widths-up (range)
  "Add up the widths of all ranges to left and up/left, etc."
  (if range
      (+ (range-widths-l (tree:prev range))
	 (range-widths-up (tree:dad range)))
      0))

(defun position (range off)
  "find absolute offset from this range+off to beginning"
  (+ off (range-widths-up range)))

;;==============================================================================
;; insert
;;
;;
(defun insert-into-pad (old new off)
  (if (zerop off)
      (tree:link-before old new); inserting at left edge...
      ;; either middle or right edge.  Must check old width to determine
      (let ((wold (width old)))
	(tree:link-after old new); middle or right edge...
	(when (>= wold off);done for right edge.
	  ;; for middle, we must pad...
	  (setf (width old) off); convert the null into left pad
	  (tree:link-after
	   new
	   (make-instance 'range :width (- wold off))))))
  (format t "widening ~A by ~A" (tree:dad new) (width new))
  (widen (tree:dad new):by (width new))
  new)

(defun insert (old new off)
  (insert-into-pad
   (if (obj old); if inserting into a real range,
       (tree:link-as-child-of; create a child pad range
	old (make-instance 'range :width (width old)))
       old)
   new off))

(defun delete (range)
  (narrow range :by (width range) )
  )

;;==============================================================================
;; Bisection structure
;;
#||(defclass bi ()
  ((range  :accessor range  :initarg :range  :initform nil)
  (offset :accessor offset :initarg :offset :initform 0 :type fixnum)
   (pos    :accessor pos    :initarg :pos    :initform 0 :type fixnum)))


(defun range-bisect (range pos)
  (let ((remaining pos))
    (labels ((prim (range)
	       (let ((q (- remaining (width range))))
		 (if (minusp q); we are in range!
		     (unless
			 (tree:for-next-until #'prim (tree:first range))
		       (return-from range-bisect (values range remaining)))
		     (progn (setf remaining q) nil)))))
      (prim range))))


(defun bisect (range remaining)
  (declare (type fixnum remaining))
					; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (format t "BISECTING ~A~&" range)
  (when range
      (let ((q (- remaining  (the fixnum (width range)))))
	(format t "...~A ~A~&" range q)
	(if (minusp q)
	    (unless (bisect (tree:first-node range) remaining)
	      (return-from bisect (values range remaining)))
	    (bisect (tree:next-node range) q))
	t)))
||#

;; Bisections are pretty obvious, except when it comes to 0-width ranges.
;; First of all, the root 0-range with a 0 offset should bisect to itself!
;; Secondly, a 0-width bisection should be found when nailed on the head,
;; and take precedence over the next range.  Ordinarily, when width=remaining, the

(defun bisect (range remaining)
  (declare (type fixnum remaining))
					; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (labels
      ((prim (range remaining)
	 (when range; if null, too far; just return nil
	   (format t "BISECTING |~A| ~A ~&" range remaining)
	   ;; Zero offset means definitely this node! But do deep in case child
	   (if (zerop remaining)
	       (return-from bisect (values (tree:go-deep range) 0))
	       (let ((q (- remaining  (the fixnum (width range)))))
		 (format t "...~A ~A~&" range q)
		 (if (minusp q) ;;negative means inside our range...
		     (unless (prim (tree:first-node range) remaining) ;maybe child?
		       (return-from bisect (values range remaining)))
		     ;; zero gets sent to select next node no matter what.
		     ;; positive means more to go.
		     (prim (tree:next-node range) q)
		     ))))))
;;    (if (zerop remaining)	(values range 0))
    (prim range remaining)))



(defun it-r (range i)
  (when (> (width range) (incf i))
      (setf range (tree:traverse-next range)
	    i 0))
  (values range i))

 (defun q (range)
     (print (tree:traverse-next range)))

(defun -insert-char (range pos)
  (widen (range-bisect range pos)))

(defun -delete-char (range pos)
  (narrow (range-bisect range pos))
)

;;-------------------------------
(defparameter *r* (tree:make-root (make-instance 'range )))
  

#||

(defparameter *stream-range* nil)
(defparameter *range-pad-start* nil)
(defmacro w-subtext (&body body)
  `(with-output-to-string (*standard-output*)
     (setf *stream-range* (make-instance 'range :dad nil)
	   *range-pad-start* 0)
     ,@body
     (setf (width *stream-range*) (file-position *standard-output*))))

(defmacro w-context (context &body body)
  `(progn
     (let ((start (file-position *standard-output*))
	   (*stream-range* (dll-insert
			    
			    (sub *stream-range*)
			    (make-instance 'range
					   :dad *stream-range*)
			    :after nil)))
       ,@body
       (setf (width *stream-range*)
	     (- (file-position *standard-output*) start)))))

(defparameter *t1*
  (w-subtext
    (w-context nil
      (w-context 1
	(format t "hello"))
      (format t "Twas brillig, and the slithy toves
Did gyre and gimble in the wabe:
All mimsy were the borogoves,
And the mome raths outgrabe."))
    (w-context (make-instance 'ctx2)
      (format t "\"Beware the Jabberwock, my son!
The jaws that bite, the claws that catch!
Beware the Jubjub bird, and shun
The frumious Bandersnatch!#\""))))
||#
