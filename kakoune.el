;;; kakoune.el --- A simulation, but not emulation, of kakoune, in emacs -*- lexical-binding: t; -*-

;; Author: Joseph Morag <jm4157@columbia.edu>
;; Version: 0.1
;; Package-Requires: ((ryo-modal "0.4") (multiple-cursors "1.4") (expand-region "0.11.0"))
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
(require 'ryo-modal)
(require 'expand-region)
(require 'multiple-cursors)

;;;###autoload
(defun kak/setup-keybinds ()
  "Set up default kakoune keybindings for normal mode."
  (interactive)
  (ryo-modal-keys
   ;; Basic keybindings
   ("a" forward-char :exit t)
   ("A" move-end-of-line :exit t)
   ("b" kak/backward-same-syntax :first '(kak/set-mark-here))
   ("B" kak/backward-same-syntax :first '(kak/set-mark-if-inactive))
   ("c" kak/d :exit t)
   ("C" kill-line :exit t)
   ("d" kak/d)
   ("D" kill-line)
   ("f" kak/select-to-char :first '(kak/set-mark-here))
   ("F" kak/select-to-char :first '(kak/set-mark-if-inactive))
   ("g" (("h" beginning-of-line)
         ("j" end-of-buffer)
         ("k" beginning-of-buffer)
         ("g" kak/gg)
         ("l" end-of-line)
         ("i" back-to-indentation)) :first '(deactivate-mark))
   ("G" (("h" beginning-of-line)
         ("j" end-of-buffer)
         ("k" beginning-of-buffer)
         ("g" kak/gg)
         ("l" end-of-line)
         ("i" back-to-indentation)) :first '(kak/set-mark-if-inactive))
   ("g f" find-file-at-point)
   ("G f" find-file-at-point)
   ("g x" kak/exchange)
   ("g X" kak/exchange-cancel)
   ("h" backward-char :first '(deactivate-mark))
   ("H" backward-char :first '(kak/set-mark-if-inactive))
   ("i" kak/insert-mode)
   ("I" back-to-indentation :exit t)
   ("j" next-line :first '(deactivate-mark))
   ("J" next-line :first '(kak/set-mark-if-inactive))
   ("k" previous-line :first '(deactivate-mark))
   ("K" previous-line :first '(kak/set-mark-if-inactive))
   ("l" forward-char :first '(deactivate-mark))
   ("L" forward-char :first '(kak/set-mark-if-inactive))
   ("o" kak/o :exit t)
   ("O" kak/O :exit t)
   ("p" kak/p)
   ("r" kak/replace-char)
   ("R" kak/replace-selection)
   ("t" kak/select-up-to-char :first '(kak/set-mark-here))
   ("T" kak/select-up-to-char :first '(kak/set-mark-if-inactive))
   ("w" forward-same-syntax :first '(kak/set-mark-here))
   ("W" forward-same-syntax :first '(kak/set-mark-if-inactive))
   ("M-w" forward-symbol :first '(kak/set-mark-here))
   ("M-W" forward-symbol :first '(kak/set-mark-if-inactive))
   ("x" kak/x)
   ("X" kak/X)
   ("y" kill-ring-save)
   ("Y" kak/Y)
   ("." kak/select-again :first '(kak/set-mark-if-inactive))
   ("M-;" exchange-point-and-mark)
   ("`" kak/downcase)
   ("~" kak/upcase)
   ("%" mark-whole-buffer)
   ("M-j" kak/join)
   ("[ [" backward-paragraph :first '(kak/set-mark-here))
   ("{ [" backward-paragraph :first '(kak/set-mark-if-inactive))
   ("] ]" forward-paragraph :first '(kak/set-mark-here))
   ("} ]" forward-paragraph :first '(kak/set-mark-if-inactive))
   (">" kak/indent-right)
   ("<" kak/indent-left)

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
   ("[" (("SPC" kak/insert-line-above)
         ("b" previous-buffer)
         ("p" kak/paste-above)))
   ("]" (("SPC" kak/insert-line-below)
         ("b" next-buffer)
         ("p" kak/paste-below)))

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
