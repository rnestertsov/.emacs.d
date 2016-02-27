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
(global-set-key (kbd "C-<right>") 'forward-word)
(global-set-key (kbd "C-<left>") 'backward-word)

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

;; start rainbow-delimiter-mode in most programming modes (Emacs 24 and above):
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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

(defun my/duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let
  
  ;; put the point in the lowest line and return
  (next-line arg))

(defun my/copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d lines%s copied" arg (if (= 1 arg) "" "s")))

(global-set-key (kbd "C-d") 'my/duplicate-line)
(global-set-key (kbd "C-c d") 'my/copy-line)

