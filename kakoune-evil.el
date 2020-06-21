;; Experimental evil integration

(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode))
(use-package evil-goggles
  :custom
  (evil-goggles-duration 0.050)
  :config
  (evil-goggles-mode))
(use-package general
  :config
  (general-evil-setup))

(general-imap "f"
  (general-key-dispatch 'self-insert-command
    :timeout 0.25
    "d" 'evil-normal-state))
(use-package expand-region)
(defun kak-evil-expand-region (count)
  (interactive "p")
  (evil-visual-char)
  (er/expand-region 1))

(defun kak-evil-x (count)
  (interactive "p")
  (evil-exit-visual-state)
  (evil-beginning-of-visual-line)
  (evil-visual-char)
  (forward-line count)
  (backward-char 1))
(defun kak-evil-X (count)
  (interactive "p")
  (evil-beginning-of-visual-line)
  (unless (use-region-p) (evil-visual-char))
  (forward-line count)
  (backward-char 1))
(defmacro kak-wrap-lowercase-motion (key)
  `(lambda (count)
     (interactive "p")
     (evil-exit-visual-state)
     (evil-visual-char)
     (,(evil-lookup-key evil-motion-state-map (kbd ,key)) count)))
(defmacro kak-wrap-uppercase-motion (key)
  `(lambda (count)
     (interactive "p")
     (unless (use-region-p) (evil-visual-char))
     (,(evil-lookup-key evil-motion-state-map (kbd ,key)) count)))
(defmacro kak-exit-first (key)
  `(lambda (count)
     (interactive "p")
     (evil-exit-visual-state)
     (,(evil-lookup-key evil-motion-state-map (kbd ,key)) count)))

(general-nmap
  "v" 'kak-evil-expand-region
  "x" 'kak-evil-x
  "X" 'kak-evil-X)
(general-nmap "SPC h" 'help-command)
(general-vmap "v" 'er/expand-region
  "x" '(lambda (count) (interactive "p") (forward-char) (kak-evil-x count))
  "X" '(lambda (count) (interactive "p")
         (forward-char) (kak-evil-X count) (forward-char))
  "(" 'self-insert-command
  ")" 'self-insert-command
  "[" 'self-insert-command
  "]" 'self-insert-command
  "{" 'self-insert-command
  "]" 'self-insert-command)
(general-define-key
 :states '(normal motion visual)
 "h" (kak-exit-first "h")
 "H" (kak-wrap-uppercase-motion "h")
 "j" (kak-exit-first "g j")
 "J" (kak-wrap-uppercase-motion "g j")
 "k" (kak-exit-first "g k")
 "K" (kak-wrap-uppercase-motion "g k")
 "l" (kak-exit-first "l")
 "L" (kak-wrap-uppercase-motion "l")
 "b" (kak-wrap-lowercase-motion "b")
 "e" (kak-wrap-lowercase-motion "e")
 "w" (kak-wrap-lowercase-motion "w")
 "M-b" (kak-wrap-lowercase-motion "B")
 "M-w" (kak-wrap-lowercase-motion "W")
 "M-e" (kak-wrap-lowercase-motion "E")
 "M-B" (kak-wrap-uppercase-motion "B")
 "M-W" (kak-wrap-uppercase-motion "W")
 "M-E" (kak-wrap-uppercase-motion "E")
 "g l" 'evil-end-of-visual-line
 "g i" 'evil-first-non-blank-of-visual-line
 "g h" 'evil-beginning-of-visual-line)
