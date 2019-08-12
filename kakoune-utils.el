;;; kakoune-utils.el --- Utilities for kakoune.el -*- lexical-binding: t; -*-

;; Author: Joseph Morag <jm4157@columbia.edu>
;;; Commentary:
;; Provides utility functions for kakoune.el

;;; Code:
(require 'cl-lib)
(require 'ryo-modal)
(require 'expand-region)
(require 'multiple-cursors)

(defun kakoune-insert-mode () "Return to insert mode."
       (interactive)
       (ryo-modal-mode 0))

(defun kakoune-set-mark-if-inactive () "Set the mark if it isn't active."
       (interactive)
       (unless (use-region-p) (set-mark (point))))

(defun kakoune-set-mark-here () "Set the mark at the location of the point."
       (interactive) (set-mark (point)))

(defun kakoune-deactivate-mark ()
  "Deactivate the mark.

For some reason, just calling (deactivate-mark) inside of a (ryo-modal-keys
call doesn't work."
  (interactive)
  (deactivate-mark))

(defun kakoune-backward-same-syntax (count)
  "Move backward COUNT times by same syntax blocks."
  (interactive "p")
  (forward-same-syntax (- count)))

(defvar kakoune-last-t-or-f ?f
  "Using t or f command sets this variable.")

(defvar-local kakoune-last-char-selected-to " "
  "This variable is updated by kakoune-select-to-char.")

(defun kakoune-select-up-to-char (arg char)
  "Select up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncSelect up to char: ")
  (setq kakoune-last-char-selected-to char)
  (setq kakoune-last-t-or-f ?t)
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-char direction)
    (unwind-protect
	    (search-forward (char-to-string char) nil nil arg)
	  (backward-char direction))
    (point)))

(defun kakoune-select-to-char (arg char)
  "Select up to, and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncSelect to char: ")
  (setq kakoune-last-char-selected-to char)
  (setq kakoune-last-t-or-f ?f)
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-char direction)
    (unwind-protect
	    (search-forward (char-to-string char) nil nil arg))
    (point)))

(defun kakoune-select-again (&optional count)
  "Expand the selection COUNT times to whatever the last 't' command was."
  (interactive "p")
  (if (eq kakoune-last-t-or-f ?t)
      (kakoune-select-up-to-char count kakoune-last-char-selected-to)
    (kakoune-select-to-char count kakoune-last-char-selected-to)))

(defun kakoune-x (count)
  "Select COUNT lines from the current line.

Note that kakoune's x doesn't behave exactly like this,
but I like this behavior better."
  (interactive "p")
  (beginning-of-line)
  (set-mark (point))
  (forward-line count))

(defun kakoune-X (count)
  "Extend COUNT lines from the current line."
  (interactive "p")
  (beginning-of-line)
  (unless (use-region-p) (set-mark (point)))
  (forward-line count))

(defun kakoune-d (count)
  "Kill selected text or COUNT chars."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-char count t)))

(defun kakoune-p (count)
  "Yank COUNT times after the point."
  (interactive "p")
  (dotimes (_ count) (save-excursion (yank))))

(defun kakoune-downcase ()
  "Downcase region."
  (interactive)
  (if (use-region-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-region (point) (+ 1 (point)))))

(defun kakoune-upcase ()
  "Upcase region."
  (interactive)
  (if (use-region-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-region (point) (1+ (point)))))

(defun kakoune-replace-char (char)
  "Replace selection with CHAR."
  (interactive "cReplace with char: ")
  (mc/execute-command-for-all-cursors
   (lambda () (interactive)
     (if (use-region-p)
         (progn (let ((region-size (- (region-end) (region-beginning))))
	              (delete-region (region-beginning) (region-end))
	              (mc/save-excursion
		           (insert-char char region-size t))))
       (progn (delete-region (point) (1+ (point)))
	          (mc/save-excursion
	           (insert-char char)))))))

(defun kakoune-replace-selection ()
  "Replace selection with killed text."
  (interactive)
  (if (use-region-p)
      (progn (delete-region (region-beginning) (region-end))
	         (yank))
    (progn (delete-region (point) (1+ (point)))
	       (yank))))

(defun kakoune-o (count)
  "Open COUNT lines under the cursor and go into insert mode."
  (interactive "p")
  (end-of-line)
  (dotimes (_ count)
    (electric-newline-and-maybe-indent)))

(defun kakoune-O (count)
  "Open COUNT lines above the cursor and go into insert mode."
  (interactive "p")
  (beginning-of-line)
  (dotimes (_ count)
    (newline)
    (forward-line -1)))

(defun kakoune-join ()
  "Join the next line to the current one."
  (interactive) (join-line 1))

(defun kakoune-Y (count)
  "Copy to the end of COUNT lines."
  (interactive "p")
  (save-excursion
    (let ((cur (point)))
      (move-end-of-line count)
      (kill-ring-save cur (point)))))

(defun kakoune-indent-right (count)
  "Indent the region or COUNT lines right to tab stop."
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly-right-to-tab-stop (region-beginning) (region-end))
             (setq deactivate-mark nil))
    (let ((beg (save-excursion (beginning-of-line) (point)))
          (end (save-excursion (forward-line count) (point))))
      (indent-rigidly-right-to-tab-stop beg end))))

(defun kakoune-indent-left (count)
  "Indent the region or COUNT lines left to tab stop."
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly-left-to-tab-stop (region-beginning) (region-end))
             (setq deactivate-mark nil))
    (let ((beg (save-excursion (beginning-of-line) (point)))
          (end (save-excursion (forward-line count) (point))))
      (indent-rigidly-left-to-tab-stop beg end))))

(defun kakoune-gg (count)
  "Go to the beginning of the buffer or the COUNTth line."
  (interactive "p")
  (goto-char (point-min))
  (when count (forward-line (1- count))))

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
        (mc/maybe-multiple-cursors-mode)))))

(provide 'kakoune-utils)
;;; kakoune-utils.el ends here
