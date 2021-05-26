;;; emacs-bn --- core functions for translating emacs to Bangla
;;; commentary:
;;; copyright: Md Arif Shaikh
;;; code:
(require 'emacs-bn-core)
(require 'battery)

(defun bn-add-preceding-zero-to-date-time (bn-date-or-time)
  "Add a preceding zero to BN-DATE-OR-TIME."
  (if (= (length bn-date-or-time) 1)
      (string-join (list "০" bn-date-or-time))
    bn-date-or-time))

(defcustom bn-date-separator "/"
  "Separator in date string in modeline."
  :type 'string
  :group 'emacs-bn)

(defcustom bn-time-separator ":"
  "Separator in time string in modeline."
  :type 'string
  :group 'emacs-bn)

(defcustom  bn-add-preceding-zero-to-date-time? t
  "Add a preceding zero to date or time string in modeline."
  :type 'boolean
  :group 'emacs-bn)

(defcustom bn-display-time-string-forms
  '((if bn-add-preceding-zero-to-date-time? (bn-add-preceding-zero-to-date-time (number-to-bn day)))
    bn-date-separator
    (month-name-to-bn monthname)
    bn-date-separator
    (number-to-bn (substring year -2))
    " "
    (number-to-bn 24-hours)
    bn-time-separator
    (bn-add-preceding-zero-to-date-time (number-to-bn minutes)))
  "Display time string in modeline in Bangla."
  :type 'list
  :group 'emacs-bn)

(defun bn-battery-update ()
  "Update battery status information in the mode line in Bangla."
  (let* ((data (and battery-status-function (funcall battery-status-function)))
         (percentage (car (read-from-string (cdr (assq ?p data))))))
    (setq battery-mode-line-string
	  (propertize (if (and battery-mode-line-format
			       (numberp percentage)
                               (<= percentage battery-mode-line-limit))
			  (progn
			    (setf (car (read-from-string (cdr (assq ?p data)))) (number-to-bn percentage))
			    (battery-format battery-mode-line-format data))
			"")
		      'face
                      (and (numberp percentage)
                           (<= percentage battery-load-critical)
                           'error)
		      'help-echo "ব্যাটারী অবস্থা সম্পর্কিত তথ্য")))
  (force-mode-line-update))

(provide 'emacs-bn)
;;; emacs-bn.el ends here
