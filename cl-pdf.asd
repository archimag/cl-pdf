;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; cl-pdf copyright 2002-2009 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(defsystem :cl-pdf
  :name "cl-pdf"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :licence "BSD like licence"
  :description "Common Lisp PDF Generation Library"
  :long-description "The cl-pdf package provides a stand-alone Common Lisp library to generate PDF files."
  :components ((:file "defpackage")
               (:file "config" :depends-on ("defpackage"))
               (:file "zlib" :depends-on ("config"))
               (:file "font-metrics"  :depends-on ("config"))
               (:file "encodings"  :depends-on ("defpackage"))
               (:file "t1-font" :depends-on ("font-metrics" "encodings"))
               (:file "ttu-font" :depends-on ("font-metrics"))
               (:file "ttf" :depends-on ("ttu-font"))
               (:file "font" :depends-on ("t1-font" "ttu-font"))
               (:file "pdf" :depends-on ("font"))
               (:file "x11-colors" :depends-on ("defpackage"))
               (:file "pdf-base" :depends-on ("pdf" "x11-colors"))
               (:file "png" :depends-on ("pdf-base"))
               (:file "pdf-geom" :depends-on ("pdf-base"))
               (:file "text" :depends-on ("pdf-base"))
               (:file "bar-codes" :depends-on ("pdf-geom"))
               (:file "chart" :depends-on ("text" "pdf-geom")))
  :depends-on (#:iterate #:salza2 #:zpb-ttf))
