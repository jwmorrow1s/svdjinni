(defconstant +xml-header-tag+ "<?xml version=\"1.0\" standalone=\"no\"?>")
(defconstant +svg-footer-tag+ "</svg>")

(declaim (ftype (function (list) boolean) string-list-p))
(defun string-list-p (lst)
  (every #'(lambda (x) (typep x 'string)) lst))

(deftype string-list ()
  `(and (satisfies consp)
        (satisfies string-list-p)))

(declaim (ftype (function (fixnum fixnum) string) svg-header-tag))
(defun svg-header-tag (width height)
  (format nil "<svg version=\"1.1\" width=\"~D\" height=\"~D\" xmlns=\"http://www.w3.org/2000/svg\">"
          width height))

(declaim (ftype (function (fixnum fixnum pathname string-list) t) with-svg))
(defun with-svg (width height destination content)
  (with-open-file (file destination :direction :output
    :if-exists :overwrite
    :if-does-not-exist :create)
    (dolist (line `(,+xml-header-tag+ ,(svg-header-tag width height) ,@content ,+svg-footer-tag+))
      (write-line line file))))

(defun main (&rest argv)
  (declare (ignorable argv))
  (with-svg 120 100 #p"test.svg" '("hello" "there")))
