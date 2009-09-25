;;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:pdf)

;;; Basic (and crude) text layout functions
;;; use cl-typesetting for nice text layout functions

(defconstant +section-char+ (code-char 167)
  "This character is not entered literally to avoid causing problems
with Lisps that read source files in UTF-8 encoding.")
(defvar *delimiter-chars* (list #\Space #\Tab #\Newline +section-char+))

(defun text-width (string font font-size)
  (loop for c across string
	summing (get-char-width c font font-size)))

(defun split-text (string font font-size max-width &optional max-height)
  (let ((max-line-number (if max-height
                             (floor (+ max-height (* 0.2 font-size))
                                    (* 1.2 font-size))))
        (current-line-number 1))
    (flet ((check-max-number-of-lines ()
             (and max-line-number
                  (< max-line-number
                     (prog1
                         current-line-number
                       (incf current-line-number))))))
      (if (> (* 2 (get-char-width #\M font font-size)) max-width)
          (loop for c across string
                until (check-max-number-of-lines)
                collect (string c))
          (let ((width 0)
                (start 0)
                (result ()))
            (loop with max-number-of-lines = (and max-line-number (< max-line-number current-line-number))
                  until max-number-of-lines
                  for i from 0
                  for c across string
                  for d = (get-char-width c font font-size) do          
                  (if (or (char= c #\Newline)
                          (char= c +section-char+)
                          (> (+ width d) max-width))
                      (progn
                        (push (string-trim *delimiter-chars* (subseq string start i)) result)
                        (setf start i width 0)
                        (setf max-number-of-lines (check-max-number-of-lines)))
                      (incf width d))
                  finally (unless max-number-of-lines
                            (push (string-trim *delimiter-chars* (subseq string start)) result)))
            (nreverse result))))))


(defun draw-centered-text (x y string font font-size &optional max-width max-height)
  (pdf:in-text-mode
   (pdf:move-text x y)
   (pdf:set-font font font-size)
   (loop with dy = (* -1.2 font-size)
	 for (str . rest) on (if max-width (split-text string font font-size max-width max-height) (list string))
	 for last-x = 0 then offset
	 for offset = (* -0.5 (text-width str font font-size)) do
	 (move-text (- offset last-x) 0)
	 (show-text str)
	 (when rest (pdf:move-text 0 dy)))))

(defun draw-left-text (x y string font font-size &optional max-width max-height)
  (pdf:in-text-mode
   (pdf:move-text x y)
   (pdf:set-font font font-size)
   (loop with dy = (* -1.2 font-size)
	 for (str . rest) on (if max-width (split-text string font font-size max-width max-height) (list string))
	 for last-x = 0 then offset
	 for offset = (- (text-width str font font-size)) do
	 (move-text (- offset last-x) 0)
	 (show-text str)
	 (when rest (pdf:move-text 0 dy)))))

(defun draw-right-text (x y string font font-size &optional max-width max-height)
  (pdf:in-text-mode
   (pdf:move-text x y)
   (pdf:set-font font font-size)
   (loop with dy = (* -1.2 font-size)
	 for (str . rest) on (if max-width (split-text string font font-size max-width max-height) (list string))
	 do
	 (show-text str)
	 (when rest (move-text 0 dy)))))
