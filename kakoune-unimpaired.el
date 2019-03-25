;;; kakoune-unimpaired.el --- "Unimpaired" plugin for kakoune.el -*- lexical-binding: t; -*-

;; Author: Joseph Morag <jm4157@columbia.edu>
;;; Commentary:
;; Provides a couple functions similar to Tim Pope's vim-unimpaired package

;;; Code:

(defun kak/insert-line-below (count)
  "Insert COUNT empty lines below the current line."
  (interactive "p")
  (save-excursion
    (end-of-line)
    (open-line count)))

(defun kak/insert-line-above (count)
  "Insert COUNT empty lines above the current line."
  (interactive "p")
  (save-excursion
    (end-of-line 0)
    (open-line count)))

(defun kak/paste-above (count)
  "Paste (yank) COUNT times above the current line."
  (interactive "p")
  (save-excursion
    (dotimes (_ count) (end-of-line 0)
	     (newline)
	     (yank))))

(defun kak/paste-below (count)
  "Paste (yank) COUNT times below the current line."
  (interactive "p")
  (save-excursion
    (dotimes (_ count) (end-of-line)
	     (newline)
	     (yank))))

(provide 'kakoune-unimpaired)
;;; kakoune-unimpaired ends here
