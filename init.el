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

;; make sure exec-path to be same as the PATH in zsh/bash config
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(;; allow ido usage in as many contexts as possible. see
                      ;; customizations/better-defaults.el line 47 for a description
                      ;; of ido
                      ido-ubiquitous

                      ;; Enhances M-x to allow easier execution of commands. Provides
                      ;; a filterable list of possible commands in the minibuffer
                      ;; http://www.emacswiki.org/emacs/Smex
                      smex

                      ;; color themes
                      color-theme
                      color-theme-solarized

                      ;; makes handling lisp expressions much, much easier
                      ;; cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
                      paredit

                      ;; key bindings and code colorization for Clojure
                      ;; https://github.com/clojure-emacs/clojure-mode
                      clojure-mode

                      ;; extra syntax highlighting for clojure
                      clojure-mode-extra-font-locking

                      ;; Clojure development environment
                      ;; https://github.com/clojure-emacs/cider
                      cider

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
                      desktop

                      ;; automatically pairs braces and quotes
                      ;; https://github.com/capitaomorte/autopair
                      autopair

                      ;; fuzzy-search utility
                      ;; https://github.com/d11wtq/grizzl
                      grizzl

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

                      ;; powerline
                      ;; https://github.com/milkypostman/powerline
                      powerline

                      ;; go-mode
                      ;; support for golang
                      go-mode

                      ;; go-eldoc
                      ;; provides eldoc for Go language
                      go-eldoc

                      ;; rust-mode
                      ;; https://github.com/rust-lang/rust/tree/master/src/etc/emacs
                      rust-mode

                      ;; hydra
                      ;; https://github.com/abo-abo/hydra
                      hydra

                      ;; ace-window
                      ;; https://github.com/abo-abo/ace-window
                      ace-window

                      ;; helm
                      ;; https://github.com/emacs-helm/helm
                      helm

                      ;; exec-path-from-shell
                      ;; https://github.com/purcell/exec-path-from-shell
                      exec-path-from-shell)

  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; add vendor folder to load path
(add-to-list 'load-path "~/.emacs.d/vendor/")

(defun my/package-list-untracked-packages ()
  "Show a list of packages that installed and are not in 'my-packages'"
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x my-packages))
                              (not (package-built-in-p x))
                              (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))

;;;;
;; Load grapviz and plantuml libraries 
;;;;

(load-library "graphviz-dot-mode")
(load-library "plantuml-mode")

;;;;
;; Configuration
;;;;

(add-to-list 'load-path "~/.emacs.d/conf")

;; Windows OS specific settings
(when (string-equal system-type "windows-nt")
  )

;; Mac OS X specific settings
(when (string-equal system-type "darwin")
  ;; make option the super key on mac
  (setq mac-option-modifier 'super)
  ;; map meta to command key for mac
  (setq ns-command-modifier 'meta))

(load "ui.el")
(load "keyboard.el")
(load "editing.el")
(load "navigation.el")
(load "misc.el")

(load "conf-autocomplete.el")
(load "conf-spellcheck.el")
(load "conf-org.el")
(load "conf-erc.el")

;; language specific
(load "lang-clojure.el")
(load "lang-c.el")
(load "lang-markdown.el")
(load "lang-go.el")
;;(load "lang-nzsql.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#005369"))))
 '(company-scrollbar-fg ((t (:background "#003f4f"))))
 '(company-tooltip ((t (:inherit default :background "#003340"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))
(put 'erase-buffer 'disabled nil)
