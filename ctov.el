;;; ctov - context overlays

;; Author: treflip <TODO@TODO.net>
;; Version: 0.1
;; URL: https://github.com/tre-flip/ctov.el
;; Package-Requires: ((ctov-modal "0.4") (multiple-cursors "1.4") (expand-region "0.11.0") (emacs "25.1"))
;; MIT License

;;; Commentary:
;; This package provides automatic context recognition, context highlighting,
;; context-dependent commands for common tasks such as copying, killing, pasting, etc.
;; Heavily influenced by xah-fly-keys and objed.
;; Read the source.

;;; Code:

(defvar ctov-excluded-modes nil)

(defface ctov-context-active
  '((t :inherit highlight :extend t))
  "Highlight active context")

(defgroup ctov nil
  "Automatic context overlays."
  :group  'editing
  :tag    "ctov"
  :prefix "ctov-"
  )

(define-minor-mode ctov-mode
  "Toggle the `ctov-mode' minor mode.
With a prefix argument ARG, enable `ctov-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'."
  nil " CTOV" nil
  (if ctov-mode
      (add-hook 'post-command-hook 'ctov-post-command)
    (remove-hook 'post-command-hook 'ctov-post-command)))

;;;###autoload
(define-globalized-minor-mode ctov-global-mode
  ctov-mode
  ctov--maybe-activate)

(defun ctov--maybe-activate ()
  "Activate `ctov-mode' if current buffer is not minibuffer or blacklisted.
This is used by `ctov-global-mode'."
  (unless (or (minibufferp)
              (member major-mode ctov-excluded-modes))
    (ctov-mode 1)))

;; this is messy
(defvar ctov-brackets nil "string of left/right brackets pairs.")
(setq ctov-brackets "()[]{}<>＜＞（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠")

(defvar ctov-left-brackets '("\""  "(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" "〘")


  "List of left bracket chars.")

(defvar ctov-right-brackets '("\"" ")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»" "〙")
  "list of right bracket chars.")

(defvar ctov-punctuation-regex nil "A regex string for the purpose of moving cursor to a punctuation.")
(setq ctov-punctuation-regex "[!\?\"\.,`'#$%&*+:;=@^|~]+")



(defun ctov-looking-back-on-line (regexp)
  "Version of `looking-back' that only checks current line."
  (looking-back regexp (line-beginning-position)))

(defun ctov-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'."
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp))
     ((looking-at (regexp-opt ctov-left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt ctov-right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

(defvar ctov-context-overlay nil
  "An overlay over current context.")

(defvar ctov-context-end-position nil
  "End of context")

(defvar ctov-context-beginning-postion nil 
  "Beginnig of context")

(defun ctov-context-end ()
  "Goto end of context"
  (message "NOT IMPLEMENTED"))

(defun ctov-context-beginning ()
  "Goto beginnig of context"
  (message "NOT IMPLEMENTED"))

(defvar ctov-conmmand-context-triggers-alist (list)
  "Defines an alist of commands that enable certain contexts")

;; FUNCTIONS FOR CONTEXT IDENTIFICATION:

;; Note that context can be set manually.
;; Automatic context recognition is skipped in this case.

(defun ctov-inside-comment-p ()
  (or (nth 4 (syntax-ppss))
      (memq (get-text-property (point) 'face) '(font-lock-comment-face font-lock-comment-delimiter-face))))

(defun ctov-looking-at-comment ()
  ;; (or (looking-at "<")
  ;;     (looking-back ">"))
  (or (looking-back "\\s!")
      (looking-back "\\s<")
      (looking-at "\\s!")
      (looking-at "\\s<")))

(defun ctov-looking-at-line ()
  (or (looking-at "$")
      (looking-at "^")
      (looking-back "^[ \t]")))

(defun ctov-looking-at-brackets ()
  (condition-case nil
      (or (looking-at (regexp-opt ctov-left-brackets))
	  (looking-back (regexp-opt ctov-right-brackets) (max (- (point) 1) 1)))  
    (error nil)))

(defun ctov-looking-at-symbol ()
  (looking-at "\\s_\\|\\sw"))

(defun ctov-looking-back-word ()
  (looking-back "\\sw"))

(defun ctov-looking-at-word ()
  (looking-at "\\sw"))

;; (defun ctov-looking-at-block ()
;;   (or (looking-at "\n[ \t]*\n")
;;       ;; this function doesn't work as expected
;;       (looking-back "\n[ \t]*\n")))


(defun ctov-looking-at-block ()
  "Block is recognized only then point at the closest empty line before it."
  (or (looking-at "^[ \t]*\n[^\n]+")))

;; FUNCTIONS FOR CONTEXT BOUNDS COMPUTATION

;; Line bounds are determined by line-end-position and line-beginning-position.
;; At this moment only single-line comments are hadled correctly

;; not all comment delimiters are captured
(defun ctov-comment-beg ()
  "Currently works with single-line comments"
  (point))
;; not all comment delimiters are captured
(defun ctov-comment-end ()
  (line-end-position))

;; TODO: rewrite to work from within sexps and strings
(defun ctov-brackets-beg ()
  (save-excursion
    (let* ((!start (point))
	   (!end   (progn (ctov-goto-matching-bracket)
			  (point))))
      (if (< !start !end)
	  !start
	!end))))

;; TODO: rewrite to work from within sexps and strings
(defun ctov-brackets-end ()
  (save-excursion
    (let* ((!start (point))
	   (!end   (progn (ctov-goto-matching-bracket)
			  (point))))
      (if (< !start !end)
	  !end
	!start))))

(defun ctov-symbol-beg ()
  (save-excursion
    (let ((symbol-regexp "\\s_\\|\\sw"))
      (when (or (looking-at symbol-regexp)
		(ctov-looking-back-on-line symbol-regexp))
	(skip-syntax-backward "_w")))
    (point)))

(defun ctov-symbol-end ()
  (save-excursion
    (let ((symbol-regexp "\\s_\\|\\sw"))
      (when (or (looking-at symbol-regexp)
		(ctov-looking-back-on-line symbol-regexp))
	(skip-syntax-forward "_w")))
    (point)))

(defun ctov-word-beg ()
  (save-excursion
    (let ((word-regexp "\\sw"))
      (when (or (looking-at word-regexp)
		(ctov-looking-back-on-line word-regexp))
	(skip-syntax-backward "w")))
    (point)))

(defun ctov-word-end ()
  (save-excursion
    (let ((word-regexp "\\sw"))
      (when (or (looking-at word-regexp)
		(ctov-looking-back-on-line word-regexp))
	(skip-syntax-forward "w")))
    (point)))

(defun ctov-block-beg ()
  (save-excursion
    (when (not (looking-at "^[ \t]*\n[^\n]+"))
      (re-search-backward "^[ \t]*\n[^\n]+" nil "move"))
    (point)))

(defun ctov-block-end ()
  (save-excursion
    (when (not (looking-at "\n[ \t]*\n"))
      (re-search-forward "\n[ \t]*\n" nil "move"))
    (point)))

;; (defun ctov-block-beg ()
;;   (save-excursion
;;     ;; if not looking at the beginnig of block, search for it
;;     (when (not (looking-back "\n[ \t]*\n"))
;;       (re-search-backward "\n[ \t]*\n" nil "move"))
;;     (point)))

;; (defun ctov-block-end ()
;;   (save-excursion
;;     ;; if not looking at the end of block, search for it
;;     (when (not (looking-at "\n[ \t]*\n"))
;;       (re-search-forward "\n[ \t]*\n" nil "move")
;;       (skip-chars-backward "\n\t "))
;;     (point)))

;; functions for context display and processing 
(defun ctov-expand-context ()
  "Expands context to the next larger one"
  (interactive)
  (message "NOT IMPLEMENTED: expansion"))

(defun ctov-contract-context ()
  "Contracts context to the next smaller one"
  (interactive)
  (message "NOT IMPLEMENTED: contraction"))

;; UPDATE AND DISPLAY CONTEXT OVERLAY
(defun ctov-update-overlay (&optional beg end) 
  (when ctov-context-overlay
    (delete-overlay ctov-context-overlay))
  (if (not (and beg end))
      (setq ctov-context-overlay nil)
    (setq ctov-context-overlay (make-overlay beg end))
    (overlay-put ctov-context-overlay 'face 'ctov-context-active)))

(defun ctov-update-context ()
  "Computes context and move overlay"
  (let ((!beg nil)
	(!end nil))
    (cond
     ;; disable overlay in minibuffer and disabled modes
     ((or (minibufferp)
          (member major-mode ctov-excluded-modes))
      nil)
     ((region-active-p)
      ;; don't compute context when region is active,
      ;; commands will use region instead of context
      nil)
     ;; if context is set manually or via some other command:
     ((ctov-looking-at-block)
      (setq !beg (ctov-block-beg)
	    !end (ctov-block-end)))
     ((ctov-looking-at-brackets)
      (setq !beg (ctov-brackets-beg)
	    !end (ctov-brackets-end)))
     ((ctov-looking-at-line)
      (setq !beg (line-beginning-position)
	    !end (line-end-position)))
     ((ctov-looking-at-comment)
      (setq !beg (ctov-comment-beg)
	    !end (ctov-comment-end)))
     ((ctov-looking-at-word)
      (setq !beg (ctov-word-beg)
	    !end (ctov-word-end)))
     ((ctov-looking-at-symbol)
      (setq !beg (ctov-symbol-beg)
	    !end (ctov-symbol-end)))
     ((ctov-looking-back-word)
      (setq !beg (ctov-word-beg)
	    !end (ctov-word-end))))
    (ctov-update-overlay !beg !end)
    (setq ctov-context-beginning-postion !beg
	  ctov-context-end-position !end)))

(defun ctov-post-command () 
  (ctov-update-context))

;; FUNCTIONS FOR CONTEXT PROCESSING

(defun ctov-apply-to-context-or-region (command &rest args)
  "For commands that take START and END args. Applies command to current context."
  (interactive)
  (if (region-active-p)
      (command-execute ))
  (apply command (nconc (list (ctov-context-beginning-postion)
			      (ctov-context-end-position))
			args)))

(defun ctov-copy ()
  (interactive)
  (ctov-apply-to-context-or-region #'kill-ring-save))

(defun ctov-kill ()
  )

(provide 'ctov)
;; ctov.el ends here
