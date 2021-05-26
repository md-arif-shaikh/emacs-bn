;;; emacs-bn --- core functions for translating emacs to Bangla
;;; commentary:
;;; copyright: Md Arif Shaikh
;;; code:
(require 'emacs-bn-core)

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

(defvar bn-display-time-string-forms)
(setq bn-display-time-string-fomrs
      '((if bn-add-preceding-zero-to-date-time? (bn-add-preceding-zero-to-date-time (number-to-bn day)))
	bn-date-separator
	(month-name-to-bn monthname)
	bn-date-separator
	(number-to-bn (substring year -2))
	" "
	(number-to-bn 24-hours)
	bn-time-separator
	(bn-add-preceding-zero-to-date-time (number-to-bn minutes))))

(provide 'emacs-bn)
;;; emacs-bn.el ends here
