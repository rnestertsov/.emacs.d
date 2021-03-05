;;; init.el ---

;;;;
;; Packages
;;;;

;; define package repositories
(require 'package)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
	           '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; load and activate emacs packages.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(;; use-package
                      use-package

                      ;; rg
                      rg

                      ;; eyebrowse
                      eyebrowse

                      ;; color themes
                      solarized-theme

                      ;; Clojure development environment
                      ;; https://github.com/clojure-emacs/cider
                      cider

                      ;; clj-refactor
                      ;; https://github.com/clojure-emacs/clj-refactor.el
                      ;; clj-refactor

                      ;; auto completion
                      ;; http://company-mode.github.io/
                      company

                      ;; company-quickhelp
                      company-quickhelp

                      ;; enable popup contextual menus when using auto-completion in your code
                      ;; https://github.com/auto-complete/popup-el
                      popup

                      ;; raindow-delimiters-mode
                      ;; https://github.com/Fanael/rainbow-delimiters
                      rainbow-delimiters

                      ;; markdown mode
                      markdown-mode

                      ;; snippets
                      yasnippet

                      ;; save the state of Emacs from one session to another
                      ;; desktop

                      ;; automatically pairs braces and quotes
                      ;; https://github.com/capitaomorte/autopair
                      autopair

                      ;; project navigation
                      ;; https://github.com/bbatsov/projectile
                      projectile

                      ;; perspective mode
                      perspective

                      ;; git integration
                      ;; cheatsheet: http://daemianmack.com/magit-cheatsheet.html
                      magit

                      ;; org-mode
                      ;; http://orgmode.org/
                      org
                      org-plus-contrib

                      ;; go-mode
                      ;; support for golang
                      go-mode

                      ;; go-eldoc
                      ;; provides eldoc for Go language
                      go-eldoc

                      ;; gotest
                      gotest

                      ;; rust-mode
                      ;; https://github.com/rust-lang/rust/tree/master/src/etc/emacs
                      ;; rust-mode

                      ;; hydra
                      ;; https://github.com/abo-abo/hydra
                      hydra

                      ;; ace-window
                      ;; https://github.com/abo-abo/ace-window
                      ;; ace-window

                      ;; exec-path-from-shell
                      ;; https://github.com/purcell/exec-path-from-shell
                      exec-path-from-shell

                      ;; restclient
                      ;; https://github.com/pashky/restclient.el
                      restclient

                      ;; web-mode
                      ;; https://github.com/fxbois/web-mode
                      web-mode

                      ;; yaml-mode
                      ;; https://github.com/yoshiki/yaml-mode
                      yaml-mode

                      ;; dockerfile-mode
                      ;; https://github.com/spotify/dockerfile-mode
                      dockerfile-mode

                      ;; ledger-mode
                      ledger-mode
                      flycheck-ledger
                      dklrt

                      ;; groovy-mode
                      ;; https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes
                      ;; groovy-mode

                      ;; racket-mode
                      ;; https://github.com/greghendershott/racket-mode
                      ;; racket-mode

                      ;; interleave
                      ;; https://github.com/rudolfochrist/interleave
                      interleave

                      ;; org-ref
                      ;; https://github.com/jkitchin/org-ref
                      org-ref

                      ;; protobuf-mode
                      ;; https://github.com/google/protobuf/blob/master/editors/protobuf-mode.el
                      protobuf-mode

                      ;; demo-it
                      ;; https://github.com/howardabrams/demo-it
                      ;; demo-it

                      ;; cquery
                      ;; https://github.com/cquery-project/cquery
                      cquery

                      ;; lsp-mode
                      lsp-mode
                      ;; lsp-ui ;; to intrusive
                      company-lsp

                      ivy
                      ivy-rich
                      counsel-projectile

                      cyberpunk-theme

                      elisp-slime-nav

                      amx

                      which-key
                      )

  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; add vendor folder to load path
(add-to-list 'load-path "~/.emacs.d/vendor/")

;; configure flycheck
;; (require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; workaround for go vet https://github.com/flycheck/flycheck/issues/1523
;; (let ((govet (flycheck-checker-get 'go-vet 'command)))
  ;; (when (equal (cadr govet) "tool")
    ;; (setf (cdr govet) (cddr govet))))

(defun my/package-list-untracked-packages ()
  "Show a list of packages that installed and are not in 'my-packages'."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x my-packages))
                              (not (package-built-in-p x))
                              (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; dired-sidebar
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file))

(use-package vterm
  :ensure t)

;; enable eyebroser mode on startup
(eyebrowse-mode t)

;;;;
; Load mu4e
;;;;
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")

;;;;
;; Load grapviz and plantuml libraries
;;;;

(load-library "graphviz-dot-mode")
(load-library "plantuml-mode")
(load-library "go-dlv")

;;;;
;; Configuration
;;;;

(add-to-list 'load-path "~/.emacs.d/conf")

;; start emacs server
(load "server")
(unless (server-running-p) (server-start))

(load "ui.el")
(load "keyboard.el")
(load "editing.el")
(load "navigation.el")
(load "misc.el")
(load "popup.el")

;; (load "mail.el")

(load "conf-autocomplete.el")
(load "conf-spellcheck.el")
(load "conf-org.el")
(load "conf-apib.el")
(load "conf-dired.el")
(load "conf-lsp.el")
(load "conf-eww.el")

;;(load "conf-erc.el")

;; language specific
;; (load "lang-clojure.el")
(add-hook 'clojure-mode-hook #'paredit-mode)


(load "lang-ts.el")
(load "lang-c.el")
(load "lang-markdown.el")
(load "lang-go.el")
;;(load "lang-nzsql.el")

(load "conf-eshell.el")

(load "ledger.el")

;;;;
;; Platform specific configurations
;;;;

;; Windows OS specific settings
(when (string-equal system-type "windows-nt")
  (setq user-init-file "C:\\Users\\rnestertsov\\AppData\\Roaming\\.emacs.d\\init.el")
  (setq w32-get-true-file-attributes nil))

;; Mac OS X specific settings
(when (string-equal system-type "darwin")
  (setq user-init-file "/Users/rnestertsov/.emacs.d/init.el")

  ;; by default mac has following keybindings
  ;;    meta = option key
  ;;    super = command key
  (setq mac-command-modifier 'meta)     ; make cmd key do Meta
  (setq mac-option-modifier 'super)     ; make opt key do Super
  (setq mac-control-modifier 'control)  ; make Control key do Control
  (setq ns-function-modifier 'hyper)    ; make Fn key do Hyper

  (defun pbcopy ()
  		(interactive)
  		(call-process-region (point) (mark) "pbcopy")
  		(setq deactivate-mark t))

	(defun pbpaste ()
  		(interactive)
  		(call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

	(global-set-key (kbd "M-c") 'pbcopy)
	(global-set-key (kbd "M-v") 'pbpaste))

;; instant access to init.el
(defun my/find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") #'my/find-user-init-file)

;; have custom as separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(put 'dired-find-alternate-file 'disabled nil)
