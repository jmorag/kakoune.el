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
(require 'ryo-modal)
(require 'expand-region)
(require 'multiple-cursors)

;; Basic keybindings
(ryo-modal-keys
 ("a" forward-char :exit t)
 ("A" move-end-of-line :exit t)
 ("b" backward-same-syntax :first '(set-mark-here))
 ("B" backward-same-syntax :first '(set-mark-if-inactive))
 ("c" kak/d :exit t)
 ("C" kill-line :exit t)
 ("d" kak/d)
 ("D" kill-line)
 ("f" kak/select-to-char :first '(set-mark-here))
 ("F" kak/select-to-char :first '(set-mark-if-inactive))
 ("g" (("h" beginning-of-line)
       ("j" end-of-buffer)
       ("k" beginning-of-buffer)
       ("g" beginning-of-buffer)
       ("l" end-of-line)
       ("i" back-to-indentation)) :first '(deactivate-mark))
 ("G" (("h" beginning-of-line)
       ("j" end-of-buffer)
       ("k" beginning-of-buffer)
       ("g" beginning-of-buffer)
       ("l" end-of-line)
       ("i" back-to-indentation)) :first '(set-mark-if-inactive))
 ("g f" find-file-at-point)
 ("G f" find-file-at-point)
 ("g x" kak/exchange)
 ("g X" kak/exchange-cancel)
 ("h" backward-char :first '(deactivate-mark))
 ("H" backward-char :first '(set-mark-if-inactive))
 ("i" ryo-leave)
 ("I" back-to-indentation :exit t)
 ("j" next-line :first '(deactivate-mark))
 ("J" next-line :first '(set-mark-if-inactive))
 ("k" previous-line :first '(deactivate-mark))
 ("K" previous-line :first '(set-mark-if-inactive))
 ("l" forward-char :first '(deactivate-mark))
 ("L" forward-char :first '(set-mark-if-inactive))
 ("o" kak/o :exit t)
 ("O" kak/O :exit t)
 ("p" kak/p)
 ("r" kak/replace-char)
 ("R" kak/replace-selection)
 ("t" kak/select-up-to-char :first '(set-mark-here))
 ("T" kak/select-up-to-char :first '(set-mark-if-inactive))
 ("w" forward-same-syntax :first '(set-mark-here))
 ("W" forward-same-syntax :first '(set-mark-if-inactive))
 ("M-w" forward-symbol :first '(set-mark-here))
 ("M-W" forward-symbol :first '(set-mark-if-inactive))
 ("x" kak/x)
 ("X" kak/X)
 ("y" kill-ring-save)
 ("Y" kak/Y)
 ("." kak/select-again :first '(set-mark-if-inactive))
 (";" deactivate-mark)
 ("M-;" exchange-point-and-mark)
 ("`" kak/downcase)
 ("~" kak/upcase)
 ("%" mark-whole-buffer)
 ("M-j" kak/join)
 ("[ [" backward-paragraph :first '(set-mark-here))
 ("{ [" backward-paragraph :first '(set-mark-if-inactive))
 ("] ]" forward-paragraph :first '(set-mark-here))
 ("} ]" forward-paragraph :first '(set-mark-if-inactive))
 )

(ryo-modal-keys  ("0" "M-0" :norepeat t)
                 ("1" "M-1" :norepeat t)
                 ("2" "M-2" :norepeat t)
                 ("3" "M-3" :norepeat t)
                 ("4" "M-4" :norepeat t)
                 ("5" "M-5" :norepeat t)
                 ("6" "M-6" :norepeat t)
                 ("7" "M-7" :norepeat t)
                 ("8" "M-8" :norepeat t)
                 ("9" "M-9" :norepeat t)
                 ("-" "M--" :norepeat t))

;; Region selectors
(ryo-modal-keys
 ("v" er/expand-region)
 ("M-i" (("w" er/mark-word)
         ("b" er/mark-inside-pairs)
         ("'" er/mark-inside-quotes)
         ("s" er/mark-text-sentence)
         ("p" er/mark-text-paragraph)))
 ("M-a" (("w" er/mark-symbol)
         ("b" er/mark-outside-pairs)
         ("'" er/mark-outside-quotes)
         ("s" er/mark-text-sentence)
         ("p" er/mark-text-paragraph))))

;; Multiple cursors
(ryo-modal-keys ("s" mc/mark-all-in-region)
                ("S" mc/split-region))

(provide 'kakoune)
;;; kakoune.el ends here
