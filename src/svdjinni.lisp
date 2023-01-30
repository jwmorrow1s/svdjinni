(defconstant +xml-header-tag+ "<?xml version=\"1.0\" standalone=\"no\"?>")
(defconstant +svg-footer-tag+ "</svg>")

;;; types
;
(declaim (ftype (function (t) boolean) ->boolean))
(defun ->boolean (thing)
  (not (not thing)))

(declaim (ftype (function (keyword) boolean) position-type-p))
(defun position-type-p (position)
  "determines whether the provided argument is a position type"
   (not (null (find position '(:absolute :relative)))))

(declaim (ftype (function (simple-vector) boolean) vec%stringp))
(defun vec%stringp (maybe-string-vector)
  (and (not (null maybe-string-vector))
       (> (length maybe-string-vector) 0)
       (every #'(lambda (ch) (typep ch 'string)) maybe-string-vector)))

(deftype vec%string ()
  "a homogenous vector of strings"
    '(and (satisfies vectorp)
          (satisfies vec%stringp)))

(deftype position-type ()
  "a position type describes the way in which the path's coordinates are being altered"
     '(and (satisfies keywordp)
           (satisfies position-type-p)))

(declaim (ftype (function (simple-vector) simple-vector) take-not-nil))
(defun take-not-nil (vec)
  (let ((count 0))
    (loop for thing across vec
          do (if (null thing) (return)
                 ;else
                 (setf count (1+ count))))
    (subseq vec 0 count)))

(declaim (ftype (function (string &key (:delimeter standard-char)) vec%string) split))
(defun split (s &optional &key (delimeter #\Space))
  "a function to split a string provided a delimiter"
  (let ((ret (make-array (list (length s)) :initial-element nil))
        (current-substring "")
        (idx 0))
    (loop for char across s
        do (if (eq delimeter char)
         (progn (setf (aref ret idx) current-substring)
                (setf idx (1+ idx))
                (setf current-substring ""))
        ;else
        (setf current-substring (format nil "~A~C" current-substring char))))
    ; make sure final value is appended
    (when (> (length current-substring) 0) (setf (aref ret idx) current-substring))
    (take-not-nil ret)))

(defstruct point
     (x 0 :type fixnum)
     (y 0 :type fixnum))

;;; @todo - finish cubic-bezier-p
;(declaim (ftype (function (string) boolean) cubic-bezier-p))
;(defun cubic-bezier-p (maybe-bezier)
;  ; minimum possible bezier expression C 0 0 0 0 0 0 is 13 chars long
;  (and (>= (length maybe-bezier) 13)
;       (or (eq #\C) (eq #\c) (elt maybe-bezier 0))))

; (deftype cubic-bezier-type ()
;   "a type of a valid bezier command"
;   '(and (satisfies stringp)
;         (satisfies cubic-bezier-p)))

;;; @todo replace string return type with cubic-bezier-type when complete
(declaim (ftype (function (point point point &key (:type position-type)) string) cubic-bezier))
(defun cubic-bezier (control1 control2 terminus &optional &key (type :absolute))
  "creates a cubic-bezier instruction for a path"
  (let ((effective-position-char (if (eq :absolute type) #\C #\c)))
      (format nil "~c ~d ~d ~d ~d ~d ~d"
        effective-position-char
        (point-x control1) (point-y control1)
        (point-x control2) (point-y control2)
        (point-x terminus) (point-y terminus))))

(declaim (ftype (function (list) boolean) list%stringp))
(defun list%stringp (lst)
  "determines if argument is a strict homogenous list of strings"
  (and (not (null lst))
   (every #'(lambda (x) (typep x 'string)) lst)))

(deftype list%string ()
  "a strict homogenous list of strings"
  '(and (satisfies consp)
        (satisfies list%stringp)))

(declaim (ftype (function (fixnum fixnum) string) svg-header-tag))
(defun svg-header-tag (width height)
  "a function which creates an svg-header-tag"
  (format nil "<svg version=\"1.1\" width=\"~D\" height=\"~D\" xmlns=\"http://www.w3.org/2000/svg\">"
          width height))

;;;


(declaim (ftype (function (fixnum fixnum pathname list%string) t) with-svg))
(defun with-svg (width height destination content)
  (with-open-file (file destination :direction :output
    :if-exists :overwrite
    :if-does-not-exist :create)
    (dolist (line `(,+xml-header-tag+ ,(svg-header-tag width height) ,@content ,+svg-footer-tag+))
      (write-line line file))))

(defun main (&rest argv)
  (declare (ignorable argv))
  (with-svg 120 100 #p"test.svg" '("hello" "there")))
