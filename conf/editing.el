;; editing.el

;; key binding to use "hippie expand" for test autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; highlight matching parenthesis
(show-paren-mode 1)

;; highlight current line
(global-hl-line-mode 1)

;; interactive search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; don't use hard tabs
(setq-default indent-tabs-mode nil)

;; tabs are 2 spaces
(setq-default tab-width 2)

;; when you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; create backups in ~/.emacs.d/backups
;; http://www.gnu.org/software/emacs/manual/html_code/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-default nil)

;; configures yasnippet
;; http://www.emacswiki.org/emacs/Yasnippet
(require 'yasnippet)
(yas/global-mode 1)

;; comments
(defun my/toggle-comment-on-line ()
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position))
  (forward-line 1))
(global-set-key (kbd "C-;") 'my/toggle-comment-on-line)

;; rainbows
;; TODO: find out why it's not found
;; (global-rainbow-delimiters-mode t)

;; use 2 spaces for tabs
(defun my/tabs-to-spaces ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; disable long lines wrap
(setq-default truncate-lines t)

;; Makes sexps flash when you eval them!
(require 'highlight)
(require 'eval-sexp-fu)
