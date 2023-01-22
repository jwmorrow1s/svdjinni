(defconstant +xml-header-tag+ "<?xml version=\"1.0\" standalone=\"no\"?>")
(defconstant +svg-header-tag+ "<svg version=\"1.1\" width=\"120\" height=\"100\" xmlns=\"http://www.w3.org/2000/svg\">")
(defconstant +svg-footer-tag+ "</svg>")

(defun do-thing ()
  (with-open-file (file #p"test.svg" :direction :output
    :if-exists :overwrite
    :if-does-not-exist :create)
    (dolist (line (list +xml-header-tag+ +svg-header-tag+ +svg-footer-tag+))
      (write-line line file))))

(defun main (&rest argv)
  (declare (ignorable argv))
  (do-thing))
