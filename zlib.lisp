;;; cl-pdf copyright 2002-2009 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:pdf)


;; (defun load-zlib (&optional force)
;;   (declare (ignore force))
;;   (setf *compress-streams* t))

;; string-to-octets copied from the original salza
(defun string-to-octets (string start end)
  "Convert STRING to a sequence of octets, if possible."
  (declare (type string string)
           ;;(type buffer-offset start end)
           (optimize (speed 3) (safety 0)))
  #+(and sbcl (not octet-characters))
  (sb-ext:string-to-octets string :external-format :iso-8859-1 :start start :end end)
  #+(and allegro (not octet-characters))
  (excl:string-to-octets string :start start :end end :null-terminate nil)
  #+(and clisp (not octet-characters))
  (ext:convert-string-to-bytes string custom:*default-file-encoding* :start start :end end)
  #+(or octet-characters lispworks)
  (let* ((length (- end start))
	 (result (make-array length :element-type 'octet)))
    (loop for i fixnum from start below end
	  for j fixnum from 0
	  do (setf (aref result j) (char-code (aref string i))))
    result)
  #+(and (not octet-characters) (not (or sbcl allegro clisp lispworks)))
  (error "Do not know how to convert a string to octets."))

(defun compress-string (string)
  (let ((input (if (stringp string)
		   (string-to-octets string 0 (length string))
		   string))
	(chunks ()))
    (flet ((cb (octet-vector end)
	     (push (subseq octet-vector 0 end)
		   chunks)))
      (let ((compressor
	     (make-instance 'salza2:zlib-compressor
	      :callback #'cb)))
	(salza2:compress-octet-vector input compressor)
	(salza2:finish-compression compressor)))
    (reverse chunks)))


(setf *compress-streams* t)