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
   ("view" "ভিউ"))
 "Cyphejor rules to show mode name in Bangla"
 :type 'alist
 :group 'bn)

(provide 'bn)
;;; bn.el ends here
