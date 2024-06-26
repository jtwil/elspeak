;;; elspeak.el --- call espeak on a region in a buffer

;; Copyright (C) 2019-2024 by Jordan Wilson

;; Author: Jordan Wilson <jordan.t.wilson@gmx.com>
;; Keywords: speech, text-to-speech
;;
;; This file is not part of GNU Emacs.
;;
;; elspeak.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; elspeak.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple Emacs package to have regions spoken aloud by espeak. This is NOT an
;; accessibility package; I wrote it primarily to avoid tired eyes after reading
;; long news articles.

;; The main commands are `elspeak-speak-region' and `elspeak-speak-buffer'.

;;; Code:

(require 'url)
(require 'gnus-art)			; for `gnus-button-url-regexp'

(defvar elspeak-executable "espeak"
  "Path of the espeak executable")

(defvar elspeak-default-speed 175
  "Speed value passed to espeak using the \"-s\" argument.
This isn't used if `elspeak-speak-string' is passed its SPEED argument.")

(defvar elspeak-show-start t
  "If non-nil, on `elspeak-speak-region' move the cursor to the beginning of the
region and scroll that line to the top of the window.")

(defconst elspeak-process-name "emacs-espeak"
  "Name of elspeak's espeak process.")

(defvar elspeak-speak-buffer-mode-funcs
  '((gnus-article-mode . elspeak-speak-buffer-gnus-article)
    (pdf-view-mode . elspeak-speak-buffer-pdf-view)
    (Info-mode . elspeak-speak-buffer-info))
  "Associated list for mode specific functions to perform the heavy-lifting for
`elspeak-speak-buffer'. The `car' of each is the mode, and the `cdr' the
associated function.")

(defun elspeak-get-process ()
  "Get the current espeak process."
  (get-process elspeak-process-name))
 
(defun elspeak--maybe-no-process ()
  "Error if no espeak process exists."
  (unless (elspeak-get-process)
    (user-error "No %s process exists" elspeak-process-name)))

(defun elspeak-speak-string (string &optional speed)
  "Speak a string. Takes STRING to be spoken. If the optional argument SPEED is
non-nil, espeak will speak at this speed, otherwise it will use the value of
`elspeak-default-speed'."
  (when (elspeak-get-process)
    (if (yes-or-no-p (format "%s process already exists, kill it and \
start this one?" elspeak-process-name))
	;; by the time the user responds, the process may have ended
	(if (elspeak-get-process)
	    (elspeak-kill 'noerr)
	  (message "%s already ended" elspeak-process-name))
      (user-error "Not starting a new %s process" elspeak-process-name)))
  ;; don't speak URLs
  (when (string-match-p gnus-button-url-regexp string)
    (setq string (elspeak-remove-urls string)))
  (let (filename)
    ;; if it's a really long string, use a file to avoid ARG_MAX and friends
    (when (> (length string) 30000)
      (setq filename (make-temp-file elspeak-process-name nil ".txt"))
      (write-region string nil filename))
    ;; start the espeak process
    (apply #'start-process
	   (append (list elspeak-process-name nil elspeak-executable
			 "-s" (number-to-string
			       (or speed elspeak-default-speed)))
		   ;; pass the file or string
		   (if filename
		       (list "-f" filename)
		     (list string))))))

(defun elspeak-speak-region (beg end &optional speed region)
  "Speak a region. Supply two arguments, character positions BEG and END
indicating the stretch of text to be spoken. If the optional argument SPEED is
non-nil, espeak will speak at this speed, otherwise it will use the value of
`elspeak-default-speed'.  If the function is called interactively or the
optional argument REGION is non-nil, the function ignores BEG and END, and
speaks the current region instead."
  (interactive (append (if (use-region-p)
			   (list (region-beginning) (region-end))
			 (user-error "No active region is set"))
		       (list current-prefix-arg t)))
  (let ((string (if region
		    ;; region may be a rectangle
		    (funcall region-extract-function nil)
                  (buffer-substring-no-properties beg end))))    
    (elspeak-speak-string string speed))
  ;; if the entire region isn't shown, scroll its first line to the top of the
  ;; screen.
  (when region ; override BEG and END when we've been passed REGION
    (setq beg (region-beginning)
	  end (region-end)))
  (when (and elspeak-show-start
	     (or (not (pos-visible-in-window-p beg))
		 (not (pos-visible-in-window-p end))))
    (goto-char beg)
    (recenter 0))
  (deactivate-mark))

(defun elspeak-remove-urls (string)
  "Replace the URLs in STRING with a notice \"Link removed: domain.name.here\"."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward gnus-button-url-regexp nil t)
      (replace-match (save-match-data
		       (elspeak-remove-url--notice (match-string 0)))))
    (buffer-string)))

(defun elspeak-remove-url--notice (string)
  (let ((www-regexp "^www\\.\\|^www[0-9]\\."))
    ;; if a URL begins with "www." etc, replace that with "http://"
    (setq string (replace-regexp-in-string www-regexp "http://" string))
    (setq string (url-host (url-generic-parse-url string)))
    ;; remove "www." etc
    (setq string (replace-regexp-in-string www-regexp "" string)))
  (concat "Link removed: " string))

(defun elspeak-speak-buffer (&optional speed override)
  "Speak the contents of the current buffer. By default this simply passes the
text between the minimum and maximum points to `elspeak-speak-region'. If the
current mode is defined in the variable `elspeak-speak-buffer-mode-funcs', the
associated function is called instead. If the optional argument OVERRIDE is
non-nil, that variable is ignored and the default behaviour used.

If the optional argument SPEED is non-nil, espeak will speak at this speed,
otherwise it will use the value of `elspeak-default-speed'."
  (interactive (list (if (integerp current-prefix-arg)
			 (prefix-numeric-value current-prefix-arg)
		       nil)))
  (let ((func (assoc major-mode elspeak-speak-buffer-mode-funcs)))
    (if (and func (not override))
	(funcall (cdr func) speed)
      (elspeak-speak-region (point-min) (point-max) speed))))

(defun elspeak-kill (&optional noerr)
  "Kill the espeak process with SIGKILL."
  (interactive)
  (unless noerr
    (elspeak--maybe-no-process))
  (when (elspeak-get-process)
    (delete-process (elspeak-get-process))
    (message "%s killed" elspeak-process-name)))

;; Non-Unix OSes don't have the SIGSTOP or SIGCONT process signals, so
;; don't define their entry functions on them
(unless (member system-type '(windows-nt ms-dos))
  (defun elspeak-pause ()
    "Pause the espeak process with SIGSTOP."
    (elspeak--maybe-no-process)
    ;; `stop-process' uses SIGSTP, that doesn't work but "SIGSTOP" does
    (signal-process (process-id (elspeak-get-process)) 'SIGSTOP))

  (defun elspeak-continue ()
    "Continue the espeak process with SIGCONT."
    (elspeak--maybe-no-process)
    (continue-process (elspeak-get-process)))
  
  (defun elspeak-pause-toggle ()
    "Pause or continue the espeak process."
    (interactive)
    (elspeak--maybe-no-process)
    (let ((status (process-status (elspeak-get-process))))
      (cond ((eq status 'stop)
	     (elspeak-continue))
	     ((eq status 'run)
	      (elspeak-pause))))))

;; Mode specific functions used by `elspeak-speak-buffer'

(defun elspeak-speak-buffer-gnus-article (&optional speed)
  "Implementation of `elspeak-speak-buffer' for gnus-article mode.  This cuts
out article headers and trailing lines starting \"URL:\" left by mail programs
such as rss2email. Optional argument SPEED is the same as in
`elspeak-speak-buffer'."
  (unless (eq major-mode 'gnus-article-mode)
    (user-error "Not a Gnus article buffer"))
  (let (beg end)
    (save-mark-and-excursion
      (goto-char (point-min))
      (set-mark (progn (search-forward-regexp "^\\([[:space:]]\\|$\\)$")
		       (forward-line) (point)))
      (goto-char (point-max))
      ;; if there's a URL line at the end, don't speak it
      (search-backward-regexp "\\(URL\:\\)" nil t)
      (setq beg (region-beginning)
	    end (region-end)))
    (elspeak-speak-region beg end speed)))

(defun elspeak-speak-buffer-info (&optional speed)
  "Implementation of `elspeak-speak-buffer' for Info-mode. This skips over the
header line at the top of an info buffer. Optional argument SPEED is the same as
in `elspeak-speak-buffer'."
  (when (not (eq major-mode 'Info-mode))
    (user-error "Not in a Info buffer"))
  (let (beg end)
    (save-mark-and-excursion
      (goto-char (point-min))
      (set-mark (progn (search-forward-regexp "^\\([[:space:]]\\|$\\)$")
		       (forward-line) (point)))
      (goto-char (point-max))
      (setq beg (region-beginning)
	    end (region-end)))
    (elspeak-speak-region beg end speed)))

(defun elspeak-speak-buffer-pdf-view (&optional speed)
  "Implementation of `elspeak-speak-buffer' for pdf-view-mode. This speaks the
whole page text. Optional argument SPEED is the same as in
`elspeak-speak-buffer'."
  (unless (eq major-mode 'pdf-view-mode)
    (user-error "Not a PDF-View buffer"))
  (elspeak-speak-string (pdf-info-gettext (pdf-view-current-page)
					  '(0 0 1 1))
			speed))

;; Mode specific functions to extract strings

(defun elspeak-speak-pdf-view-region (&optional speed)
  "Send the `pdf-view' region selected to `elspeak-speak-string'. See that
function's documentation for a description of SPEED."
  (interactive)
  (when (not (eq major-mode 'pdf-view-mode))
    (user-error "Not in a pdf-view buffer"))
  (if (pdf-view-active-region-p)
      (elspeak-speak-string (car (pdf-view-active-region-text)) speed)
    (user-error "No active pdf-view region")))

(provide 'elspeak)

;;; elspeak.el ends here
