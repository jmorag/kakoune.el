;;; kakoune.el --- A simulation, but not emulation, of kakoune -*- lexical-binding: t; -*-

;; Author: Joseph Morag <jm4157@columbia.edu>
;; Version: 0.1
;; URL: https://github.com/jmorag/kakoune.el
;; Package-Requires: ((ryo-modal "0.45") (multiple-cursors "1.4") (expand-region "0.11.0") (emacs "25.1"))
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
(require 'kakoune-shell-commands)
(require 'cl-lib)
(require 'ryo-modal)
(require 'expand-region)
(require 'multiple-cursors)

;;;###autoload
(defun kakoune-setup-keybinds ()
  "Set up default kakoune keybindings for normal mode."
  (global-subword-mode 1)
  (ryo-modal-keys
   (:mc-all t)
   ;; Region selectors
   ("M-i" (("w" er/mark-word)
           ("b" er/mark-inside-pairs)
           ("'" er/mark-inside-quotes)))
   ("M-a" (("w" er/mark-symbol)
           ("b" er/mark-outside-pairs)
           ("'" er/mark-outside-quotes))))
  ;; this works now but I've gotten used to not having it
  ;; (ryo-modal-major-mode-keys
  ;;  'prog-mode
  ;;  ("b" kakoune-backward-same-syntax :first '(kakoune-set-mark-here) :mc-all t)
  ;;  ("B" kakoune-backward-same-syntax :first '(kakoune-set-mark-if-inactive) :mc-all t)
  ;;  ("w" forward-same-syntax :first '(kakoune-set-mark-here) :mc-all t)
  ;;  ("W" forward-same-syntax :first '(kakoune-set-mark-if-inactive) :mc-all t))
  (ryo-modal-keys
   ;; Basic keybindings
   (:mc-all t)
   ("a" forward-char :exit t)
   ("A" move-end-of-line :exit t)
   ("b" backward-word :first '(kakoune-set-mark-here))
   ("B" backward-word :first '(kakoune-set-mark-if-inactive))
   ("c" kakoune-d :exit t)
   ("C" kill-line :exit t)
   ("d" kakoune-d)
   ("D" kill-line)
   ("f" kakoune-select-to-char :first '(kakoune-set-mark-here))
   ("F" kakoune-select-to-char :first '(kakoune-set-mark-if-inactive))
   ("g" (("h" beginning-of-line)
         ("<left>" beginning-of-line)
         ("j" end-of-buffer)
         ("<down>" end-of-buffer)
         ("k" beginning-of-buffer)
         ("<up>" beginning-of-buffer)
         ("g" kakoune-gg)
         ("l" end-of-line)
         ("<right>" end-of-line)
         ("i" back-to-indentation)) :first '(kakoune-deactivate-mark))
   ("G" (("h" beginning-of-line)
         ("<left>" beginning-of-line)
         ("j" end-of-buffer)
         ("<down>" end-of-buffer)
         ("k" beginning-of-buffer)
         ("<up>" beginning-of-buffer)
         ("g" kakoune-gg)
         ("l" end-of-line)
         ("<right>" end-of-line)
         ("i" back-to-indentation)) :first '(kakoune-set-mark-if-inactive))
   ("g f" find-file-at-point)
   ("G f" find-file-at-point)
   ("g x" kakoune-exchange)
   ("g X" kakoune-exchange-cancel)
   ("h" backward-char :first '(kakoune-deactivate-mark))
   ("H" backward-char :first '(kakoune-set-mark-if-inactive))
   ("i" kakoune-insert-mode)
   ("I" back-to-indentation :exit t)
   ("j" next-line :first '(kakoune-deactivate-mark))
   ("J" next-line :first '(kakoune-set-mark-if-inactive))
   ("k" previous-line :first '(kakoune-deactivate-mark))
   ("K" previous-line :first '(kakoune-set-mark-if-inactive))
   ("l" forward-char :first '(kakoune-deactivate-mark))
   ("L" forward-char :first '(kakoune-set-mark-if-inactive))
   ("o" kakoune-o :exit t)
   ("O" kakoune-O :exit t)
   ("p" kakoune-p)
   ("r" kakoune-replace-char)
   ("R" kakoune-replace-selection)
   ("t" kakoune-select-up-to-char :first '(kakoune-set-mark-here))
   ("T" kakoune-select-up-to-char :first '(kakoune-set-mark-if-inactive))
   ("w" forward-word :first '(kakoune-set-mark-here))
   ("W" forward-word :first '(kakoune-set-mark-if-inactive))
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
   ("] ]" forward-paragraph :first '(kakoune-set-mark-here))
   (">" kakoune-indent-right)
   ("<" kakoune-indent-left)

   ;; Treat arrow keys the same as "hjkl"
   ("<down>" next-line :first '(kakoune-deactivate-mark))
   ("<S-down>" next-line :first '(kakoune-set-mark-if-inactive))
   ("<up>" previous-line :first '(kakoune-deactivate-mark))
   ("<S-up>" previous-line :first '(kakoune-set-mark-if-inactive))
   ("<right>" forward-char :first '(kakoune-deactivate-mark))
   ("<S-right>" forward-char :first '(kakoune-set-mark-if-inactive))
   ("<left>" backward-char :first '(kakoune-deactivate-mark))
   ("<S-left>" backward-char :first '(kakoune-set-mark-if-inactive))

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
         ("p" kakoune-paste-above)))
   ("]" (("SPC" kakoune-insert-line-below)
         ("p" kakoune-paste-below)))

   ;; Multiple cursors
   ("s" mc/mark-all-in-region)
   ("S" mc/split-region)

   ;; Shell commands
   ("|" kakoune-shell-pipe)
   ("!" kakoune-shell-command))

  ;; put these here because they shouldn't be repeated for all cursors
  (ryo-modal-keys
   ("[ b" previous-buffer)
   ("] b" next-buffer)))

;;;###autoload
(defun kakoune-setup-keybinds-colemak-dh ()
  "Set up default kakoune keybindings for normal mode colemak-dh
layout to preserve muscle memory.

|--------+---------|
| QWERTY | Colemak |
|--------+---------|
| h      | n       |
| j      | e       |
| k      | i       |
| l      | o       |
| n      | j       |
| e      | k       |
| i      | l       |
| o      | h       |
|--------+---------|
"
  (global-subword-mode 1)
  (ryo-modal-keys
   (:mc-all t)
   ;; Region selectors
   ("M-i" (("w" er/mark-word)
           ("b" er/mark-inside-pairs)
           ("'" er/mark-inside-quotes)))
   ("M-a" (("w" er/mark-symbol)
           ("b" er/mark-outside-pairs)
           ("'" er/mark-outside-quotes))))
  ;; new layout, let's try this again.
  (ryo-modal-major-mode-keys
   'prog-mode
   ("b" kakoune-backward-same-syntax :first '(kakoune-set-mark-here) :mc-all t)
   ("B" kakoune-backward-same-syntax :first '(kakoune-set-mark-if-inactive) :mc-all t)
   ("w" forward-same-syntax :first '(kakoune-set-mark-here) :mc-all t)
   ("W" forward-same-syntax :first '(kakoune-set-mark-if-inactive) :mc-all t))
  (ryo-modal-keys
   ;; Basic keybindings
   (:mc-all t)
   ("a" forward-char :exit t)
   ("A" move-end-of-line :exit t)
   ("b" backward-word :first '(kakoune-set-mark-here))
   ("B" backward-word :first '(kakoune-set-mark-if-inactive))
   ("c" kakoune-d :exit t)
   ("C" kill-line :exit t)
   ("d" kakoune-d)
   ("D" kill-line)
   ("f" kakoune-select-to-char :first '(kakoune-set-mark-here))
   ("F" kakoune-select-to-char :first '(kakoune-set-mark-if-inactive))
   ("g" (("n" beginning-of-line)
         ("<left>" beginning-of-line)
         ("e" end-of-buffer)
         ("<down>" end-of-buffer)
         ("i" beginning-of-buffer)
         ("<up>" beginning-of-buffer)
         ("g" kakoune-gg)
         ("o" end-of-line)
         ("<right>" end-of-line)
         ("l" back-to-indentation)) :first '(kakoune-deactivate-mark))
   ("G" (("n" beginning-of-line)
         ("<left>" beginning-of-line)
         ("e" end-of-buffer)
         ("<down>" end-of-buffer)
         ("i" beginning-of-buffer)
         ("<up>" beginning-of-buffer)
         ("g" kakoune-gg)
         ("o" end-of-line)
         ("<right>" end-of-line)
         ("l" back-to-indentation)) :first '(kakoune-set-mark-if-inactive))
   ("g f" find-file-at-point)
   ("G f" find-file-at-point)
   ("g x" kakoune-exchange)
   ("g X" kakoune-exchange-cancel)
   ("n" backward-char :first '(kakoune-deactivate-mark))
   ("N" backward-char :first '(kakoune-set-mark-if-inactive))
   ("l" kakoune-insert-mode)
   ("L" back-to-indentation :exit t)
   ("e" next-line :first '(kakoune-deactivate-mark))
   ("E" next-line :first '(kakoune-set-mark-if-inactive))
   ("i" previous-line :first '(kakoune-deactivate-mark))
   ("I" previous-line :first '(kakoune-set-mark-if-inactive))
   ("o" forward-char :first '(kakoune-deactivate-mark))
   ("O" forward-char :first '(kakoune-set-mark-if-inactive))
   ("h" kakoune-o :exit t)
   ("H" kakoune-O :exit t)
   ("p" kakoune-p)
   ("r" kakoune-replace-char)
   ("R" kakoune-replace-selection)
   ("t" kakoune-select-up-to-char :first '(kakoune-set-mark-here))
   ("T" kakoune-select-up-to-char :first '(kakoune-set-mark-if-inactive))
   ("w" forward-word :first '(kakoune-set-mark-here))
   ("W" forward-word :first '(kakoune-set-mark-if-inactive))
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
   ("] ]" forward-paragraph :first '(kakoune-set-mark-here))
   (">" kakoune-indent-right)
   ("<" kakoune-indent-left)

   ;; Treat arrow keys the same as "hjkl"
   ("<down>" next-line :first '(kakoune-deactivate-mark))
   ("<S-down>" next-line :first '(kakoune-set-mark-if-inactive))
   ("<up>" previous-line :first '(kakoune-deactivate-mark))
   ("<S-up>" previous-line :first '(kakoune-set-mark-if-inactive))
   ("<right>" forward-char :first '(kakoune-deactivate-mark))
   ("<S-right>" forward-char :first '(kakoune-set-mark-if-inactive))
   ("<left>" backward-char :first '(kakoune-deactivate-mark))
   ("<S-left>" backward-char :first '(kakoune-set-mark-if-inactive))

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
         ("p" kakoune-paste-above)))
   ("]" (("SPC" kakoune-insert-line-below)
         ("p" kakoune-paste-below)))

   ;; Multiple cursors
   ("s" mc/mark-all-in-region)
   ("S" mc/split-region)

   ;; Shell commands
   ("|" kakoune-shell-pipe)
   ("!" kakoune-shell-command))

  ;; put these here because they shouldn't be repeated for all cursors
  (ryo-modal-keys
   ("[ b" previous-buffer)
   ("] b" next-buffer)))
(provide 'kakoune)
;;; kakoune.el ends here
