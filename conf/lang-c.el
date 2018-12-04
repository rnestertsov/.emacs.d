;;; lang-c.el --

;;

(require 'cquery)
(setq cquery-executable "/usr/local/bin/cquery")

(defun my/c-mode-hook ()
  "Hook for C/C++ mode"
  (lsp-cquery-enable)
  ;; (setq cquery-sem-highlight-method 'font-lock)
  ;; alternatively, (setq cquery-sem-highlight-method 'overlay)

  ;; For rainbow semantic highlighting
  ;; (cquery-use-default-rainbow-sem-highlight)
  )

(add-hook 'c-mode-common-hook 'my/c-mode-hook)

;; (require 'cc-mode)
;; (require 'autopair)

;; activate ecb
;; (require 'ecb)
;; (require 'ecb-autoloads)

;; (setq stack-trace-on-error t)

;; (defun my/c-mode-hook ()
;;   "Hook for C/C++ mode"
;;   ;; style configuration
;;   (c-set-style "linux")
;;   (setq tab-width 4)
;;   (setq indent-tabs-mode t)
;;   ;; common hook
;;   (local-set-key "\C-c:" 'uncomment-region)
;;   (local-set-key "\C-c;" 'comment-region)
;;   (local-set-key "\C-c\C-c" 'comment-region)
;;   (font-lock-mode 1)
;;   ;; autopairing
;;   (autopair-global-mode 1)
;;   (setq autopair-autowrap t)
;;   ;; local keys
;;   (local-set-key [return] 'newline-and-indent))
