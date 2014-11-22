;;; lang-c.el --

;;

(require 'cc-mode)
(require 'autopair)

;; activate ecb
(require 'ecb)
(require 'ecb-autoloads)

(setq stack-trace-on-error t)

(defun my/c-mode-hook ()
  "Hook for C/C++ mode"
  ;; style configuration
  (c-set-style "linux")
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  ;; common hook
  (local-set-key "\C-c:" 'uncomment-region)
  (local-set-key "\C-c;" 'comment-region)
  (local-set-key "\C-c\C-c" 'comment-region)
  (font-lock-mode 1)
  ;; autopairing
  (autopair-global-mode 1)
  (setq autopair-autowrap t)
  ;; local keys
  (local-set-key [return] 'newline-and-indent))
(add-hook 'c-mode-common-hook 'my/c-mode-hook)


