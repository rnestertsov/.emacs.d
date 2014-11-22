;;; yasnippet.el ---

;; Configures yasnippet

(require 'yasnippet)
(setq yas/snippet-dirs (append '("~/.emacs.d/snippets/") yas/snippet-dirs))
(yas/global-mode 1)

(add-to-list 'auto-mode-alist '("/snippets/" . snippet-mode))
