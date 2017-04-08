;;;; package.lisp

(defpackage #:cl-range
  (:nicknames :range)
  (:use #:cl)
  (:shadow cl:position cl:delete)
  (:export
   #:range #:obj #:width
   #:narrow #:widen
   #:position
   #:insert #:delete #:bisect
   ))

 
