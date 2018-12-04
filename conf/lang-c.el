;;; lang-c.el --

;;

(require 'cquery)
(setq cquery-executable "/usr/local/bin/cquery")

(defun my/c-mode-hook ()
  "Hook for C/C++ mode"
  (lsp-cquery-enable))

(add-hook 'c-mode-common-hook 'my/c-mode-hook)
