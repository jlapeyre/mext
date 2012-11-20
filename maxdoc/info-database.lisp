(in-package :info-database)
(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))

(defun autoload-maxima-index ()
  ;; Autoload the index, but make sure we use a sensible *read-base*.
  ;; See bug 1951964.  GCL doesn't seem to have
  ;; with-standard-io-syntax.  Is just binding *read-base* enough?  Is
  ;; with-standard-io-syntax too much for what we want?
  #-gcl
  (with-standard-io-syntax
    (maxima::mfuncall 'cause-maxima-index-to-load))
  #+gcl
  (let ((*read-base* 10.))
    (maxima::mfuncall 'cause-maxima-index-to-load)))

(defun read-info-text (x)
  (declare (special maxima::*maxima-infodir* maxima::*maxima-lang-subdir*))
  (let* ((value x)
	 (filename (car value))
	 (byte-offset (cadr value))
	 (byte-count (caddr value))
	 (text (make-string byte-count))
	 (subdir-bit
	  (if (null maxima::*maxima-lang-subdir*)
	      ""
	      (concatenate 'string "/" maxima::*maxima-lang-subdir*)))
	 (path+filename (concatenate 'string maxima::*maxima-infodir* subdir-bit "/" filename)))
    (with-open-file (in path+filename :direction :input)
      (file-position in byte-offset)
      #+gcl (gcl-read-sequence text in :start 0 :end byte-count)
      #-gcl (read-sequence text in :start 0 :end byte-count))
    text))

#+gcl
(defun gcl-read-sequence (s in &key (start 0) (end nil))
  (dotimes (i (- end start))
    (setf (aref s i) (read-char in))))

(defun search-key (key item)
  "Called by doc-system interface."
  (declare (ignore item))
  key)

(defun str-item-name (item)
  "Print item name. Called by doc-system interface."
  (let ((heading-title (fifth (second item))))
    (format nil "~a~@[  (~a)~]"
              (car (second item)) heading-title)))

(defun str-item (item)
  "Print item. Called by doc-system interface."
 (format nil "~A~%~%" (read-info-text item)))

(unless (doc-system::ds-registered-p "info-deffn-defvr")
  (doc-system::ds-make-and-register
   :name "info-deffn-defvr"
   :data cl-info::*info-deffn-defvr-hashtable*
   :search-key-func #'search-key
   :str-item-func #'str-item
   :str-item-name-func #'str-item-name
   :full-match-arg t)) ;; last arg is a hack because these tables don't fit the doc system

(unless (doc-system::ds-registered-p "info-section")
  (doc-system::ds-make-and-register
   :name "info-section"
   :data cl-info::*info-section-hashtable*
   :search-key-func #'search-key
   :str-item-func #'str-item
   :str-item-name-func #'str-item-name
   :full-match-arg t))
