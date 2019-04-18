;;; kakoune-utils.el --- Utilities for kakoune.el -*- lexical-binding: t; -*-

;; Author: Joseph Morag <jm4157@columbia.edu>
;;; Commentary:
;; Provides utility functions for kakoune.el

;;; Code:
(defun kak/insert-mode () "Return to insert mode."
       (interactive)
       (ryo-modal-mode 0))
(defun kak/set-mark-if-inactive () "Set the mark if it isn't active."
       (interactive)
       (unless (use-region-p) (set-mark (point))))
(defun kak/set-mark-here () "Set the mark at the location of the point."
       (interactive) (set-mark (point)))
(defun kak/deactivate-mark ()
  "Deactivate the mark.

For some reason, just calling (deactivate-mark) inside of a (ryo-modal-keys
call doesn't work."
  (interactive)
  (deactivate-mark))

(defun kak/backward-same-syntax (count)
  "Move backward COUNT times by same syntax blocks."
  (interactive "p")
  (forward-same-syntax (- count)))

(defvar kak/last-t-or-f ?f
  "Using t or f command sets this variable.")
(defvar kak/last-char-selected-to " "
  "This variable is updated by kak/select-to-char.")

(defun kak/select-up-to-char (arg char)
  "Select up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncSelect up to char: ")
  (setq kak/last-char-selected-to char)
  (setq kak/last-t-or-f ?t)
  (let ((direction (if (>= arg 0) 1 -1)))
    (progn
      (forward-char direction)
      (unwind-protect
	  (search-forward (char-to-string char) nil nil arg)
	(backward-char direction))
      (point))))

(defun kak/select-to-char (arg char)
  "Select up to, and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncSelect to char: ")
  (setq kak/last-char-selected-to char)
  (setq kak/last-t-or-f ?f)
  (let ((direction (if (>= arg 0) 1 -1)))
    (progn
      (forward-char direction)
      (unwind-protect
	  (search-forward (char-to-string char) nil nil arg))
      (point))))

(defun kak/select-again (&optional count)
  "Expand the selection COUNT times to whatever the last 't' command was."
  (interactive "p")
  (if (eq kak/last-t-or-f ?t)
      (kak/select-up-to-char count kak/last-char-selected-to)
    (kak/select-to-char count kak/last-char-selected-to)))

(defun kak/x (count)
  "Select COUNT lines from the current line.

Note that kakoune's x doesn't behave exactly like this,
but I like this behavior better."
  (interactive "p")
  (beginning-of-line)
  (set-mark (point))
  (forward-line count))

(defun kak/X (count)
  "Extend COUNT lines from the current line."
  (interactive "p")
  (beginning-of-line)
  (unless (use-region-p) (set-mark (point)))
  (forward-line count))

(defun kak/d (count)
  "Kill selected text or COUNT chars."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-char count t)))

(defun kak/p (count)
  "Yank COUNT times after the point."
  (interactive "p")
  (dotimes (_ count) (save-excursion (yank)))
  )

(defun kak/downcase ()
  "Downcase region."
  (interactive)
  (if (use-region-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-region (point) (+ 1 (point)))
    ))

(defun kak/upcase ()
  "Upcase region."
  (interactive)
  (if (use-region-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-region (point) (1+ (point)))
    ))

(defun kak/replace-char (char)
  "Replace selection with CHAR."
  (interactive "cReplace with char: ")
  (if (use-region-p)
      (progn (let ((region-size (- (region-end) (region-beginning))))
	       (delete-region (region-beginning) (region-end))
	       (save-excursion
		 (insert-char char region-size t))))
    (progn (delete-region (point) (1+ (point)))
	   (save-excursion
	     (insert-char char)))))

(defun kak/replace-selection ()
  "Replace selection with killed text."
  (interactive)
  (if (use-region-p)
      (progn (delete-region (region-beginning) (region-end))
	     (yank))
    (progn (delete-region (point) (1+ (point)))
	   (yank))))

(defun kak/o (count)
  "Open COUNT lines under the cursor and go into insert mode."
  (interactive "p")
  (end-of-line)
  (dotimes (_ count)
    (electric-newline-and-maybe-indent)))

(defun kak/O (count)
  "Open COUNT lines above the cursor and go into insert mode."
  (interactive "p")
  (beginning-of-line)
  (dotimes (_ count)
    (newline)
    (forward-line -1)))

(defun kak/join ()
  "Join the next line to the current one."
  (interactive) (join-line 1))

(defun kak/Y (count)
  "Copy to the end of COUNT lines."
  (interactive "p")
  (save-excursion
    (let ((cur (point)))
      (move-end-of-line count)
      (kill-ring-save cur (point)))))

(defun kak/indent-right (count)
  "Indent the region or COUNT lines right to tab stop."
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly-right-to-tab-stop (region-beginning) (region-end))
             (setq deactivate-mark nil))
    (let ((beg (save-excursion (beginning-of-line) (point)))
          (end (save-excursion (forward-line count) (point))))
      (indent-rigidly-right-to-tab-stop beg end))
    ))

(defun kak/indent-left (count)
  "Indent the region or COUNT lines left to tab stop."
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly-left-to-tab-stop (region-beginning) (region-end))
             (setq deactivate-mark nil))
    (let ((beg (save-excursion (beginning-of-line) (point)))
          (end (save-excursion (forward-line count) (point))))
      (indent-rigidly-left-to-tab-stop beg end))
    ))

(defun kak/gg (count)
  "Go to the beginning of the buffer or the COUNTth line"
  (interactive "p") (if count (goto-line count) (beginning-of-buffer)))

(require 'multiple-cursors)
;; Until this function is accepted upstream, we inline it here
(defun mc/split-region (beg end search)
  "Split region each time SEARCH occurs between BEG and END.

This can be thought of as an inverse to `mc/mark-all-in-region'."
  (interactive "r\nsSplit on: ")
  (let ((case-fold-search nil))
    (if (string= search "")
        (user-error "Empty search term")
      (progn
        (mc/remove-fake-cursors)
        (goto-char beg)
        (push-mark beg)
        (while (search-forward search end t)
          (save-excursion
            (goto-char (match-beginning 0))
            (mc/create-fake-cursor-at-point))
          (push-mark (match-end 0)))
        (unless (= (point) end)
          (goto-char end))
        (mc/maybe-multiple-cursors-mode)
        ))))

(provide 'kakoune-utils)
;;; kakoune-utils.el ends here
