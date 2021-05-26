;;; emacs-bn-core --- core functions for translating emacs to Bangla
;;; commentary:
;;; copyright: Md Arif Shaikh
;;; code:
(defvar digit-alist)
(setq digit-alist '(("0" . "০")
		    ("1" . "১")
		    ("2" . "২")
		    ("3" . "৩")
		    ("4" . "৪")
		    ("5" . "৫")
		    ("6" . "৬")
		    ("7" . "৭")
		    ("8" . "৮")
		    ("9" . "৯")))

(defun digit-to-bn (digit)
  "Translate DIGIT to Bangla."
  (cdr (assoc (cond ((numberp digit) (number-to-string digit))
		    ((stringp digit) digit))
	      digit-alist)))

(defun numberstring-to-digits (numberstring)
  "Get a list of strings of the digits in a NUMBERSTRING."
  (if (= (length numberstring) 0)
      '()
    (cons (substring numberstring 0 1) (numberstring-to-digits (substring numberstring 1 (length numberstring))))))

(defun number-to-bn (number)
  "Translate a NUMBER to Bangla.  Remember that in the process any preceding zeros are lost.  This is due to the fact that the 'number-to-string remove preceding zeros."
  (let* ((numberstring (cond ((numberp number) (number-to-string number))
			     ((stringp number) number)))
	 (digits (numberstring-to-digits numberstring)))
    (mapconcat 'digit-to-bn digits "")
    ))

(defcustom day-name-bn-alist nil
  "Assoc list of day names."
  :type 'alist
  :group 'emacs-bn)

(setq day-name-bn-alist '(("Mon" . "সোম")
			  ("Tue" . "মঙ্গল")
			  ("Wed" . "বুধ")
			  ("Thu" . "বৃহস্পতি")
			  ("Fri" . "শুক্র")
			  ("Sat" . "শনি")
			  ("Sun" . "রবি")))

(defun day-name-to-bn (day-name)
  "Translate DAY-NAME to Bangla."
  (cdr (assoc day-name day-name-bn-alist)))

(defcustom month-name-bn-alist nil
  "Assoc list of month names."
  :type 'alist
  :group 'emacs-bn)

(setq month-name-bn-alist '(("Jan" . "জানুয়ারী")
			    ("Feb" . "ফেব্রুয়ারী")
			    ("Mar" . "মার্চ")
			    ("Apr" . "এপ্রিল")
			    ("May" . "মে")
			    ("Jun" . "জুন")
			    ("Jul" . "জুলাই")
			    ("Aug" . "আগস্ট")
			    ("Sep" . "সেপ্টেম্বর")
			    ("Oct" . "অক্টোবর")
			    ("Nov" . "নভেম্বর")
			    ("Dec" . "ডিসেম্বর")))

(defun month-name-to-bn (month-name)
  "Translate MONTH-NAME to Bangla."
  (cdr (assoc month-name month-name-bn-alist)))

(provide 'emacs-bn-core)
;;; emacs-bn-core.el ends here
