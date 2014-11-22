;;; lang-markdown.el ---

;; configure markdown-mode

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdn"   . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown"    . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md"       . markdown-mode))
