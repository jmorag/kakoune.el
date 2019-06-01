;;; kakoune.el --- A simulation, but not emulation, of kakoune -*- lexical-binding: t; -*-

;; Author: Joseph Morag <jm4157@columbia.edu>
;; Version: 0.1
;; URL: https://github.com/jmorag/kakoune.el
;; Package-Requires: ((ryo-modal "0.4") (multiple-cursors "1.4") (expand-region "0.11.0") (emacs "24.3"))
;; MIT License

;;; Commentary:
;; This package provides many, but not all of the editing primitives in the kakoune editor.
;; Unlike evil-mode for vim, this is very shallow emulation, and seeks to do as little
;; work as possible, leveraging Emacs native editing commmands and the work of other
;; packages wherever possible.

;;; Code:
(require 'kakoune-utils)
(require 'kakoune-exchange)
(require 'kakoune-unimpaired)
(require 'cl-lib)
(require 'ryo-modal)
(require 'expand-region)
(require 'multiple-cursors)

;;;###autoload
(defun kakoune-setup-keybinds ()
  "Set up default kakoune keybindings for normal mode."
  (interactive)
  (ryo-modal-keys
   ;; Basic keybindings
   ("a" forward-char :exit t)
   ("A" move-end-of-line :exit t)
   ("b" kakoune-backward-same-syntax :first '(kakoune-set-mark-here))
   ("B" kakoune-backward-same-syntax :first '(kakoune-set-mark-if-inactive))
   ("c" kakoune-d :exit t)
   ("C" kill-line :exit t)
   ("d" kakoune-d)
   ("D" kill-line)
   ("e" forward-word :first '(kakoune-set-mark-here))
   ("E" forward-word :first '(kakoune-set-mark-if-inactive))
   ("f" kakoune-select-to-char :first '(kakoune-set-mark-here))
   ("F" kakoune-select-to-char :first '(kakoune-set-mark-if-inactive))
   ("g" (("h" beginning-of-line)
         ("j" end-of-buffer)
         ("k" beginning-of-buffer)
         ("g" kakoune-gg)
         ("l" end-of-line)
         ("i" back-to-indentation)) :first '(deactivate-mark))
   ("G" (("h" beginning-of-line)
         ("j" end-of-buffer)
         ("k" beginning-of-buffer)
         ("g" kakoune-gg)
         ("l" end-of-line)
         ("i" back-to-indentation)) :first '(kakoune-set-mark-if-inactive))
   ("g f" find-file-at-point)
   ("G f" find-file-at-point)
   ("g x" kakoune-exchange)
   ("g X" kakoune-exchange-cancel)
   ("h" backward-char :first '(deactivate-mark))
   ("H" backward-char :first '(kakoune-set-mark-if-inactive))
   ("i" kakoune-insert-mode)
   ("I" back-to-indentation :exit t)
   ("j" next-line :first '(deactivate-mark))
   ("J" next-line :first '(kakoune-set-mark-if-inactive))
   ("k" previous-line :first '(deactivate-mark))
   ("K" previous-line :first '(kakoune-set-mark-if-inactive))
   ("l" forward-char :first '(deactivate-mark))
   ("L" forward-char :first '(kakoune-set-mark-if-inactive))
   ("o" kakoune-o :exit t)
   ("O" kakoune-O :exit t)
   ("p" kakoune-p)
   ("r" kakoune-replace-char)
   ("R" kakoune-replace-selection)
   ("t" kakoune-select-up-to-char :first '(kakoune-set-mark-here))
   ("T" kakoune-select-up-to-char :first '(kakoune-set-mark-if-inactive))
   ("w" forward-same-syntax :first '(kakoune-set-mark-here))
   ("W" forward-same-syntax :first '(kakoune-set-mark-if-inactive))
   ("M-w" forward-symbol :first '(kakoune-set-mark-here))
   ("M-W" forward-symbol :first '(kakoune-set-mark-if-inactive))
   ("x" kakoune-x)
   ("X" kakoune-X)
   ("y" kill-ring-save)
   ("Y" kakoune-Y)
   ("." kakoune-select-again :first '(kakoune-set-mark-if-inactive))
   ("M-;" exchange-point-and-mark)
   ("`" kakoune-downcase)
   ("~" kakoune-upcase)
   ("%" mark-whole-buffer)
   ("M-j" kakoune-join)
   ("[ [" backward-paragraph :first '(kakoune-set-mark-here))
   ("{ [" backward-paragraph :first '(kakoune-set-mark-if-inactive))
   ("] ]" forward-paragraph :first '(kakoune-set-mark-here))
   ("} ]" forward-paragraph :first '(kakoune-set-mark-if-inactive))
   (">" kakoune-indent-right)
   ("<" kakoune-indent-left)

   ;; Numeric arguments
   ("0" "M-0" :norepeat t)
   ("1" "M-1" :norepeat t)
   ("2" "M-2" :norepeat t)
   ("3" "M-3" :norepeat t)
   ("4" "M-4" :norepeat t)
   ("5" "M-5" :norepeat t)
   ("6" "M-6" :norepeat t)
   ("7" "M-7" :norepeat t)
   ("8" "M-8" :norepeat t)
   ("9" "M-9" :norepeat t)
   ("-" "M--" :norepeat t)

   ;; Unimpaired-like functionality
   ("[" (("SPC" kakoune-insert-line-above)
         ("b" previous-buffer)
         ("p" kakoune-paste-above)))
   ("]" (("SPC" kakoune-insert-line-below)
         ("b" next-buffer)
         ("p" kakoune-paste-below)))

   ;; Region selectors
   ("M-i" (("w" er/mark-word)
           ("b" er/mark-inside-pairs)
           ("'" er/mark-inside-quotes)
           ("s" er/mark-text-sentence)
           ("p" er/mark-text-paragraph)))
   ("M-a" (("w" er/mark-symbol)
           ("b" er/mark-outside-pairs)
           ("'" er/mark-outside-quotes)
           ("s" er/mark-text-sentence)
           ("p" er/mark-text-paragraph)))

   ;; Multiple cursors
   ("s" mc/mark-all-in-region)
   ("S" mc/split-region)))

(provide 'kakoune)
;;; kakoune.el ends here
