;;; atp --- "auto thing at point"

;; Author: treflip
;; Version: 0.1
;; URL: https://github.com/tre-flip/atp
;; Package-Requires: ((emacs "25.1"))
;; License: GPLv3

;;; Commentary:
;; This package provides automatic recognition and highlighting of things at point,
;; an interface for writing commands that operate on things and commands
;; for copying, killing, pasting things as an example of use of this interface.
;; Inspired by xah-fly-keys, expand-region and objed.
;; Works best when combined with modal editing.

;;; Code:

(require 'thingatpt)

;; enable lexical binding
(setq lexical-binding t)

;; MODE DEFINITION:

(defgroup atp nil
  "Auto thing at point"
  :group  'atp
  :tag    "atp"
  :prefix "atp-")

(defvar atp-include-newline t
  "If t, include newline in overlay when whole line is highlighted.
Default value is t.")

(defvar atp-excluded-modes nil
  "Overlay is disabled in modes from this list.")

(defvar atp-known-commands '(kill-region kill-ring-save)
  "A list of commands known to atp. Each of these commands must operate on region and allow deactivation of region.")

(defvar atp-known-repeatable-commands '(er/expand-region)
  "A list of commands known to atp. Each of these commands must operate on region.")

(defface atp-thing-active
  '((t :inherit highlight :extend t))
  "Used for highlighting active thing.")

(define-minor-mode atp-mode
  "Toggle the `atp-mode' minor mode.
With a prefix argument ARG, enable `atp-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'."
  nil " ATP" nil
  (if atp-mode
      (progn
	(add-hook 'post-command-hook 'atp-post-command)
	(add-hook 'pre-command-hook 'atp-pre-command))
    (remove-hook 'post-command-hook 'atp-post-command)
    (atp-draw-overlay)))

;;;###autoload
(define-globalized-minor-mode atp-global-mode
  atp-mode
  atp--maybe-activate)

(defun atp--maybe-activate ()
  "Activate `atp-mode' if not in minibuffer or excluded mode.
This is used by `atp-global-mode'."
  (unless (or (minibufferp)
              (member major-mode atp-excluded-modes))
    (atp-mode 1)))

;; REGEX HELPERS:

(defun atp-looking-back-on-line (regexp)
  "Version of `looking-back' that only checks current line.
REGEXP is a regular expression."
  ;; borrowed from expand-region
  (looking-back regexp (line-beginning-position)))

;; THING VARIABLES:

(defvar atp-thing-overlay nil
  "An overlay over current thing.")

;; FUNCTIONS FOR ACCESSING OVERLAY:
(defun atp-thing-end-position ()
  "End of thing."
  (when atp-thing-overlay
      (overlay-end atp-thing-overlay)))

(defun atp-thing-beginning-position ()
  "Beginnig of thing."
  (when atp-thing-overlay
    (overlay-start atp-thing-overlay)))

;; DISPLAY OVERLAY:
(defun atp-draw-overlay (&optional beg end)
  "Draw an overlay and save it to `atp-thing-overlay'.
If BEG or END is nil, delete overlay.
Returns t if overlay was drawn, nil if was deleted."
  (when atp-thing-overlay
    (delete-overlay atp-thing-overlay)
    nil)
  (if (not (and beg end))
      (setq atp-thing-overlay nil)
    (setq atp-thing-overlay (make-overlay beg end))
    (overlay-put atp-thing-overlay 'face 'atp-thing-active)
    t))

;; THING IDENTIFICATION AND OVERLAY PLACEMENT:
(defun atp-try-whole-buffer ()
  "If looking at the beginnig or end of buffer, highlight it."
  (when (or (bobp) (eobp))
    (atp-draw-overlay (point-min)
		      (point-max))))

(defun atp-try-defun ()
  "If looking at defun, highlight it."
  (let (org-element bounds)
    (setq bounds (bounds-of-thing-at-point 'defun))
    (when (and bounds
	       (eq (point)
		   (car bounds)))
      (atp-draw-overlay (car bounds)
			(cdr bounds)))))

(defun atp-try-paragraph ()
  "If looking at paragraph, highlight it."
  (let (org-element bounds)
    (setq bounds (bounds-of-thing-at-point 'paragraph))
    (when (and bounds
	       (eq (point)
		   (car bounds)))
      (atp-draw-overlay (car bounds)
			(cdr bounds)))))

(defun atp-try-line ()
  "If looking at the beginning or end of line, hightlight it if not lookig at pair."
  (when (and (or (looking-at "$")
		 (looking-back "^[ \t]*" nil))
	     (not (or (looking-at "\\s(\\|\\s\"")
		      (atp-looking-back-on-line "\\s)\\|\\s\""))))
    (atp-draw-overlay (line-beginning-position)
		      (if atp-include-newline
			  (+ 1 (line-end-position))
			(line-end-position)))))

(defun atp-try-sexp ()
  "If looking at sexp highlight it."
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (when (and bounds
	       (or (eql (point) (car bounds))
		   (eql (point) (cdr bounds))
		   (eql (cdr bounds) ; handle empty sexps like ()
			(+ 2 (car bounds)))))
      (atp-draw-overlay (car bounds)
			(cdr bounds)))))

(defun atp-try-whitespace ()
  "If looking at whitespace highlight it."
  (let ((bounds (bounds-of-thing-at-point 'whitespace)))
    (when bounds
      (message "WHITESPACE") ; remove debug
      (atp-draw-overlay (car bounds)
			(cdr bounds)))))

;; this is slow
(defun atp-try-url ()
  "If looking at url, highlight it."
  (let ((bounds (bounds-of-thing-at-point 'url)))
    (when bounds
      (atp-draw-overlay (car bounds)
			(cdr bounds)))))

(defun atp-try-word ()
  "If looking at word, highlight it."
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (atp-draw-overlay (car bounds)
			(cdr bounds)))))

(defun atp-try-comment ()
  "If looking at comment, highlight it."
  (let ((bounds (bounds-of-thing-at-point 'comment)))
    (when (and bounds
	       (looking-at "\\s!\\|\\s<\\|\\s>"))
      (message "COMMENT")
      (atp-draw-overlay (car bounds)
			(cdr bounds)))))

(defvar atp-functions nil
  "An ordered list of functions.
Each function from this list tries to identify the thing at point
and highlight it.  Functions are called successively by
`atp-update-thing' unitl one of them returns a non nil value.
This variable is reset to `atp-default-functions' after each call
to `atp-update-thing'")

(defvar atp-default-functions
  (list #'atp-try-whole-buffer
	#'atp-try-defun
	#'atp-try-paragraph
	#'atp-try-line
	#'atp-try-sexp
	#'atp-try-url
	#'atp-try-word
	#'atp-try-whitespace
	#'atp-try-comment
	;; if none of these functions work, remove overlay
	#'atp-draw-overlay)
  "An ordered list of functions.
Each function from this list tries to identify the thing at point
and highlight it.  Functions are called successively by
`atp-update-thing' unitl one of them returns a non nil value.
This is the default value for `atp-functions', that is restored
after each call to `atp-update-thing'.")

(defun atp-add-functions (&rest functions)
  "Temporaryly append a list of FUNCTIONS at the beginning of `atp-functions'."
  ;; NOT THE BEST WAY TO CONCATENATE LISTS!!!
  (setq atp-functions (nconc functions (copy-sequence atp-default-functions))))

(defun atp-restore-functions ()
  "Reset `atp-functions' to its default value."
  (setq atp-functions atp-default-functions))

;; RECOGNIZE AND DISPLAY OVERLAY

(defun atp-update-thing ()
  "Identify thing at point and draw an overlay."
  ;; don't draw when
  (if (or (minibufferp) ;; in minibuffer
	(member major-mode atp-excluded-modes) ;; in excluded mode
	(region-active-p)) ;; region is active
      (atp-draw-overlay)
    ;; successively execute functions in atp-functions until one of them returns non-nil
    (let ((funcs atp-functions)
	  success)
      (while (and funcs (not success))
	(setq success (funcall (pop funcs)))))
    (atp-restore-functions)))

;; FUNCTIONS FOR THING PROCESSING
(defun atp-apply (command &rest args)
  "Apply COMMAND to highlighted thing or region.
COMMAND is expected to have at least 2 first positional args: START and END.
The rest of its arguments are passed into ARGS."
  ;; If overlay is active, apply to overlay. If not, command-execute.
  (interactive)
  (cond ((and (atp-thing-beginning-position)
	      (atp-thing-end-position))
	 (apply command
		  (atp-thing-beginning-position)
		  (atp-thing-end-position)
		  args))
	((region-active-p)
	 (apply command (region-beginning) (region-end) args))
	(t ; this is probably incorrect if there is no active region and no atp overlay
	 (command-execute command))))

(defun atp-mark ()
  "Mark hightlighted thing as region."
  (interactive)
  (let ((!start (atp-thing-beginning-position))
	(!end (atp-thing-end-position)))
    (when (and !start !end)
      (set-mark !start)
      (goto-char !end))))

(defun atp-pre-command ()
  "Executed before every command when atp mode in active."
  (when (or (member this-command atp-known-commands)
	    (member this-command atp-known-repeatable-commands))
    (atp-mark)))

(defun atp-post-command ()
  "Executed after every command when atp mode in active."
  (when (member this-command atp-known-commands)
    (deactivate-mark))
  (atp-update-thing))

(defun atp-copy ()
  "Copy highlighted thing."
  (interactive)
  (atp-apply #'kill-ring-save))

(defun atp-kill ()
  "Kill highlighted thing."
  (interactive)
  (atp-apply #'kill-region)
  ;; (when (and (not (eq (point) (line-beginning-position)))
  ;; 	     (looking-at "[ \t]+"))
  ;;   (just-one-space))
  )

;; FIXME: comment doesn't work when point is at the end of line
(defun atp-toggle-comment ()
  "Uncomment highlighted thing if looking at comment delimimter, else comment thing."
  (interactive)
  (let (bounds)
    (setq bounds (bounds-of-thing-at-point 'comment))
    (if (and bounds
	     (eql (point) (car bounds))
	     (not (region-active-p)))
	(atp-apply #'uncomment-region)
      (atp-apply #'comment-or-uncomment-region))))

(defun atp-toggle-case ()
  "Toggle letter case inside region or highlighted thing."
  ;; based on xah-toggle-letter-case
  (interactive)
  (let ((deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (atp-thing-beginning-position)
	    $p2 (atp-thing-end-position)))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0)))))

(defun atp-replace-from-kill-ring ()
  "Delete highlighted thing and yank."
  (interactive)
  (atp-apply #'(lambda (beg end)
		 (interactive)
		 (delete-region beg end)
		 (yank))))

(provide 'atp)
;;; atp.el ends here
