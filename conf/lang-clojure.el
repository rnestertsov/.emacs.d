;;; lang-clojure.el --

;;

(defun my/clojure-switch-project (project-root)
  "Setup Clojure project root and start NREPL"
  (interactive (list (read-directory-name "Project Root: "
                                          (locate-dominating-file default-directory "project.clj"))))
  (cider-quit)
  (when (equal current-prefix-arg nil)
    (mapc 'kill-buffer (buffer-list)))
  (cd project-root)
  (cider-jack-in))

(defun my/comment-sexp ()
  "Comment out the sexp at point."
  (interactive)
  (save-excursion
    (mark-sexp)
    (paredit-comment-dwim)))

(defun my/clojure-mode-hook ()
  "Hook for Clojure mode"
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (local-set-key (kbd "C-c /") 'my/comment-sexp))
(add-hook 'clojure-mode-hook 'my/clojure-mode-hook)
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)

;; Cider configuration
(require 'cider)
(defun my/cider-mode-hook ()
  (setq nrepl-hide-special-buffers t
        cider-repl-pop-to-buffer-on-connect nil
        cider-popup-stacktraces t
        cider-auto-select-error-buffer t
        nrepl-buffer-name-separator "-"
        nrepl-buffer-name-show-port t
        ;; Make C-c C-z switch to the CIDER REPL buffer in the current window
        cider-repl-display-in-current-window t
        ;; Limit the number of items of each collection the printer
        ;; will print to 100
        cider-repl-print-length 100
        ;; Prevent C-c C-k from prompting to save the file
        ;; corresponding to the buffer being loaded; if it's modified
        cider-prompt-save-file-on-load nil
        ;; Change the result prefix for REPL eval
        cider-repl-result-prefix ";; => "
        cider-interactive-eval-result-prefix ";; => "
        ;; Font-lock code in the REPL
        cider-repl-use-clojure-font-lock t
        )
  )
(add-hook 'cider-mode-hook 'my/cider-mode-hook)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; set clojure-mode for boot files
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

(setq cider-repl-display-in-current-window nil)
