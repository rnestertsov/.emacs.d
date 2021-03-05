
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
         (lsp-mode . (lambda ()
                       (let ((lsp-keymap-prefix "C-c l"))
                         (lsp-enable-which-key-integration)))))
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
;; (use-package lsp-ui
  ;; :ensure t
  ;; :commands lsp-ui-mode)
