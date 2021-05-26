;;; bn --- core functions for translating emacs to Bangla
;;; commentary:
;;; copyright: Md Arif Shaikh
;;; code:
(require 'bn-core)
(require 'battery)
(require 'doom-modeline)
(require 'cyphejor)

(defun bn-add-preceding-zero-to-date-time (bn-date-or-time)
  "Add a preceding zero to BN-DATE-OR-TIME."
  (if (= (length bn-date-or-time) 1)
      (string-join (list "০" bn-date-or-time))
    bn-date-or-time))

(defcustom bn-date-separator "/"
  "Separator in date string in modeline."
  :type 'string
  :group 'bn)

(defcustom bn-time-separator ":"
  "Separator in time string in modeline."
  :type 'string
  :group 'bn)

(defcustom  bn-add-preceding-zero-to-date-time? t
  "Add a preceding zero to date or time string in modeline."
  :type 'boolean
  :group 'bn)

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
  :group 'bn)

(defun bn-battery-update ()
  "Update battery status information in the mode line in Bangla."
  (let* ((data (and battery-status-function (funcall battery-status-function)))
         (percentage (car (read-from-string (cdr (assq ?p data))))))
    (setq battery-mode-line-string
	  (propertize (if (and battery-mode-line-format
			       (numberp percentage)
                               (<= percentage battery-mode-line-limit))
			  (progn
			    (setf (cdr (assq ?p data)) (number-to-bn percentage))
			    (battery-format battery-mode-line-format data))
			"")
		      'face
                      (and (numberp percentage)
                           (<= percentage battery-load-critical)
                           'error)
		      'help-echo "ব্যাটারী অবস্থা সম্পর্কিত তথ্য")))
  (force-mode-line-update))


(defun bn-doom-modeline-update-flycheck-text (&optional status)
  "Update flycheck text via STATUS."
  (setq doom-modeline--flycheck-text
        (when-let
            ((text
              (pcase status
                ('finished  (when flycheck-current-errors
                              (let-alist (doom-modeline--flycheck-count-errors)
                                (if doom-modeline-checker-simple-format
                                    (doom-modeline-checker-text
                                     (number-to-bn (+ .error .warning .info))
                                     (cond ((> .error 0) 'doom-modeline-urgent)
                                           ((> .warning 0) 'doom-modeline-warning)
                                           (t 'doom-modeline-info)))
                                  (format "%s/%s/%s"
                                          (doom-modeline-checker-text (number-to-bn .error)
                                                                      'doom-modeline-urgent)
                                          (doom-modeline-checker-text (number-to-bn .warning)
                                                                      'doom-modeline-warning)
                                          (doom-modeline-checker-text (number-to-bn .info)
                                                                      'doom-modeline-info))))))
                ('running     nil)
                ('no-checker  nil)
                ('errored     (doom-modeline-checker-text "ভুল" 'doom-modeline-urgent))
                ('interrupted (doom-modeline-checker-text "বাধাপ্রাপ্ত" 'doom-modeline-debug))
                ('suspicious  (doom-modeline-checker-text "সন্দেহজনক" 'doom-modeline-urgent))
                (_ nil))))
          (propertize
           text
           'help-echo (pcase status
                        ('finished
                         (concat
                          (when flycheck-current-errors
                            (let-alist (doom-modeline--flycheck-count-errors)
                              (format "ভুল: %s, সতর্কতা: %s, তথ্য: %s\n" (number-to-bn .error) (number-to-bn .warning) (number-to-bn .info))))
                          "মাউস-১: সব ভুলগুলি দেখাও
মাউস-২: পরবর্তী ভুল"
                          (if (featurep 'mwheel)
                              "\nwheel-up/wheel-down: প্রাক/পরবর্তী ভুল")))
                        ('running "চলমান...")
                        ('no-checker "কোন চেকার নেই")
                        ('errored "ভুল")
                        ('interrupted "বাধাপ্রাপ্ত")
                        ('suspicious "সন্দেহজনক"))
           'mouse-face 'mode-line-highlight
           'local-map (let ((map (make-sparse-keymap)))
                        (define-key map [mode-line mouse-1]
                          #'flycheck-list-errors)
                        (define-key map [mode-line mouse-3]
                          #'flycheck-next-error)
                        (when (featurep 'mwheel)
                          (define-key map (vector 'mode-line
                                                  mouse-wheel-down-event)
                            (lambda (event)
                              (interactive "e")
                              (with-selected-window (posn-window (event-start event))
                                (flycheck-previous-error 1))))
                          (define-key map (vector 'mode-line
                                                  mouse-wheel-up-event)
                            (lambda (event)
                              (interactive "e")
                              (with-selected-window (posn-window (event-start event))
                                (flycheck-next-error 1))))
                          map))))))

(defun bn-doom-modeline-update-battery-status ()
  "Update battery status."
  (setq doom-modeline--battery-status
	(when (bound-and-true-p display-battery-mode)
	  (let* ((data (and battery-status-function
			    (functionp battery-status-function)
			    (funcall battery-status-function)))
		 (charging? (string-equal "AC" (cdr (assoc ?L data))))
		 (percentage (car (read-from-string (or (cdr (assq ?p data)) "ERR"))))
		 (valid-percentage? (and (numberp percentage)
					 (>= percentage 0)
					 (<= percentage battery-mode-line-limit)))
		 (face (if valid-percentage?
			   (cond (charging? 'doom-modeline-battery-charging)
				 ((< percentage battery-load-critical) 'doom-modeline-battery-critical)
				 ((< percentage 25) 'doom-modeline-battery-warning)
				 ((< percentage 95) 'doom-modeline-battery-normal)
				 (t 'doom-modeline-battery-full))
			 'doom-modeline-battery-error))
		 (icon (if valid-percentage?
			   (cond (charging?
				  (doom-modeline-icon 'alltheicon "battery-charging" "🔋" "+"
						      :face face :height 1.4 :v-adjust -0.1))
				 ((> percentage 95)
				  (doom-modeline-icon 'faicon "battery-full" "🔋" "-"
						      :face face :v-adjust -0.0575))
				 ((> percentage 70)
				  (doom-modeline-icon 'faicon "battery-three-quarters" "🔋" "-"
						      :face face :v-adjust -0.0575))
				 ((> percentage 40)
				  (doom-modeline-icon 'faicon "battery-half" "🔋" "-"
						      :face face :v-adjust -0.0575))
				 ((> percentage battery-load-critical)
				  (doom-modeline-icon 'faicon "battery-quarter" "🔋" "-"
						      :face face :v-adjust -0.0575))
				 (t (doom-modeline-icon 'faicon "battery-empty" "🔋" "!"
							:face face :v-adjust -0.0575)))
			 (doom-modeline-icon 'faicon "battery-empty" "⚠" "N/A"
					     :face face :v-adjust -0.0575)))
		 (text (if valid-percentage? (format "%s%%%%" (number-to-bn percentage)) ""))
		 (help-echo (if (and battery-echo-area-format data valid-percentage?)
				(battery-format battery-echo-area-format data)
			      "Battery status not available")))
	    (cons (propertize icon 'help-echo help-echo)
		  (propertize text 'face face 'help-echo help-echo))))))

(defcustom bn-cyphejor-rules
 '(:upcase
   ("bookmark"    "→")
   ("help" "হেল্প")
   ("org" "অর্গ")
   ("agenda" "এজেন্ডা")
   ("magit" "ম্যাগিট")
   ("status" "স্ট্যাটাস")
   ("buffer"      "বাফার")
   ("diff"        "ডিফ")
   ("dired"       "ডায়ার্ড")
   ("emacs"       "ইমাক্স")
   ("inferior"    "i" :prefix)
   ("interaction" "ইন্টারেক্শন" :prefix)
   ("interactive" "ইন্টারেক্টিভ" :prefix)
   ("lisp"        "লিস্প" :postfix)
   ("menu"        "▤" :postfix)
   ("mode"        "")
   ("package"     "↓")
   ("python"      "পাইথন")
   ("shell"       "শেল" :postfix)
   ("eshell" "ইশেল")
   ("text"        "টেক্স্ট")
   ("latex" "লেটেক্স")
   ("pdf" "পিডিএফ")
   ("view" "ভিউ")
   ("message" "মেসেজ"))
 "Cyphejor rules to show mode name in Bangla."
 :type 'alist
 :group 'bn)

(defun bn-appt-mode-line (min-to-app &optional abbrev)
  "Return an appointment string suitable for use in the mode-line.
MIN-TO-APP is a list of minutes, as strings.  If ABBREV is non-nil, abbreviates some text."
  ;; All this silliness is just to make the formatting slightly nicer.
  (let* ((multiple (> (length min-to-app) 1))
	 (imin (if (or (not multiple)
		       (not (delete (car min-to-app) min-to-app)))
		   (car min-to-app))))
    (format "%s%s %s"
	    (if abbrev "এপয়েন্টমেন্ট" "এপয়েন্টমেন্ট")
	    (if multiple "স" "")
	    (if (equal imin "0") "এখন "
	      (format "%s %s"
		      (or (number-to-bn imin) (mapconcat #'identity (mapcar #'number-to-bn min-to-app) ","))
		      (if abbrev "মিনিটে "
			(format "মিনিটে " (if (equal imin "1") "" ""))))))))

(defun bn-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date t))
	 (day (cadr date))
	 (day-of-week (calendar-day-of-week date))
	 (month (car date))
	 (monthname (calendar-month-name month t))
	 (year (nth 2 date))
	 (iso-week (org-days-to-iso-week
		    (calendar-absolute-from-gregorian date)))
	 (weekyear (cond ((and (= month 1) (>= iso-week 52))
			  (1- year))
			 ((and (= month 12) (<= iso-week 1))
			  (1+ year))
			 (t year)))
	 (weekstring (if (= day-of-week 1)
			 (format " (সপ্তাহ %02s)" (number-to-bn iso-week))
		       "")))
    (format "%s %s %s %s%s"
	    (day-name-to-bn dayname) (number-to-bn day) (month-name-to-bn monthname) (number-to-bn year) weekstring)))

(defcustom bn-org-agenda-overriding-header "আজকের কর্মসূচী:\n-------------"
  "Org-agenda-overriding-header in Bangla."
  :type 'string
  :group 'bn)

(defun bn-org-time (time-string)
  "Format TIME-STRING for 'org-agenda'."
  (let* ((hr (string-to-number (nth 0 (split-string time-string ":"))))
	 (mn (string-to-number (nth 1 (split-string time-string ":"))))
	 (bn-hr)
	 (bn-mn)
	 (part-of-day "সকাল"))
    (cond ((and (> hr 12) (< hr 17))
	   (setq part-of-day "বিকেল")
	   (setq hr (- hr 12)))
	  ((and (>= hr 17) (< hr 21))
	   (setq part-of-day "সন্ধ্যে")
	   (setq hr (- hr 12)))
	  ((>= hr 21)
	   (setq part-of-day "রাত্রি ")
	   (setq hr (- hr 12)))
	  ((= hr 12) (setq part-of-day "দুপুর ")))
    (if (< mn 10)
	(setq bn-mn (string-join (list "০" (number-to-bn mn))))
      (setq bn-mn (number-to-bn mn))
      )
    (if (< hr 10)
	(setq bn-hr (string-join (list "০" (number-to-bn hr))))
      (setq bn-hr (number-to-bn hr))
      )
    (format "%s %s%s%s " part-of-day bn-hr ":" bn-mn)))

(defun bn-org-agenda-prefix-format ()
  "Prefix format for 'org-agenda' in Bangla."
  (let ((scheduled (org-get-scheduled-time (point)))
	(deadline (org-get-deadline-time (point))))
    (if (or scheduled deadline)
	(cond (scheduled (bn-org-time (format-time-string "%H:%M" scheduled)))
	      (deadline (bn-org-time (format-time-string "%H:%M" deadline))))
      (bn-org-time (format-time-string "%H:%M")))))

(defcustom bn-org-agenda-scheduled-leaders
  '("পূর্ব নির্ধারিত:  " "%2dx নির্ধারিত: ")
  "Org agenda scheduled leaders in Bangla."
  :type 'cons
  :group 'bn)

(defcustom bn-org-agenda-deadline-leaders
  '("শেষ তারিখ:   " "%2d দিনের মধ্যে: "  "%2d দিন আগে: ")
  "Org agenda deadline leaders in Bangla."
  :type 'cons
  :group 'bn)

(defcustom bn-org-todo-keywords
  '((sequence "করুন(k)" "|" "করা হয়ে গেছে(K)" "বাতিল করা হয়েছে(B)")
    (sequence "যোগ দিন(j)" "|" "যোগ দেওয়া হয়েছে(J)" "যোগ দিতে পারিনি(U)")
    (sequence "মিটিং(m)" "|" "মিটিং হয়ে গেছে(M)" "পিছোন হয়েছে(P)")
    (sequence "পড়ুন(p)" "|" "পড়া হয়ে গেছে(S)")
    (sequence "আলোচনা(a)" "|" "আলোচনা করা হয়েছে(A)"))
  "TODO keywords in Bangla."
  :type 'cons
  :group 'bn)

(defcustom bn-org-todo-keyword-faces
  '(
    ("করুন" . (:foreground "orange" :underline t :box nil  :weight extrabold))
    ("যোগ দিন" . ( :foreground "orange" :underline t :box nil  :weight extrabold))
    ("মিটিং" . ( :foreground "orange" :underline t :box nil  :weight extrabold))
    ("পড়ুন" . ( :foreground "orange" :underline t :box nil  :weight extrabold))
    ("আলোচনা" . ( :foreground "orange" :underline t :box nil  :weight extrabold))
    ("বাতিল করা হয়েছে" . ( :foreground "gray50" :underline t :box nil))
    ("করা হয়ে গেছে" . ( :foreground "gray50" :underline t :box nil))
    ("যোগ দেওয়া হয়েছে" . ( :foreground "gray50" :underline t :box nil))
    ("মিটিং হয়ে গেছে" . ( :foreground "gray50" :underline t :box nil))
    ("পিছোন হয়েছে" . ( :foreground "gray50" :underline t :box nil))
    ("পড়া হয়ে গেছে" . ( :foreground "gray50" :underline t :box nil))
    ("আলোচনা করা হয়েছে" . ( :foreground "gray50" :underline t)))
  "Faces for TODO keywords."
  :type 'cons
  :group 'bn)


(provide 'bn)
;;; bn.el ends here
