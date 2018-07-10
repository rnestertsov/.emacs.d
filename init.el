;;; init.el ---

;;;;
;; Packages
;;;;

;; define package repositories
(require 'package)
(add-to-list 'package-archives
            '("org" . "http://orgmode.org/elpa/") t)
;;(add-to-list 'package-archives
;;	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
            '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; load and activate emacs packages.
(package-initialize)

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
                      solarized-theme

                      ;; key bindings and code colorization for Clojure
                      ;; https://github.com/clojure-emacs/clojure-mode
                      ;; clojure-mode

                      ;; extra syntax highlighting for clojure
                      ;; clojure-mode-extra-font-locking

                      ;; clj-refactor
                      ;; https://github.com/clojure-emacs/clj-refactor.el
                      ;; clj-refactor

                      ;; Clojure development environment
                      ;; https://github.com/clojure-emacs/cider
                      ;; cider

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
                      exec-path-from-shell

                      ;; restclient
                      ;; https://github.com/pashky/restclient.el
                      restclient

                      ;; web-mode
                      ;; https://github.com/fxbois/web-mode
                      web-mode

                      ;; haskell-mode
                      ;; https://github.com/haskell/haskell-mode
                      haskell-mode

                      ;; yaml-mode
                      ;; https://github.com/yoshiki/yaml-mode
                      yaml-mode

                      ;; dockerfile-mode
                      ;; https://github.com/spotify/dockerfile-mode
                      dockerfile-mode

                      ;; ensime
                      ;; http://ensime.org/editors/emacs/install/
                      ensime

                      ;; terraform-mode
                      ;; https://github.com/syohex/emacs-terraform-mode
                      terraform-mode

                      ;; ledger-mode
                      ledger-mode
                      flycheck-ledger
                      dklrt

                      ;; api-blueprint
                      ;; https://github.com/w-vi/apib-mode
                      apib-mode

                      ;; groovy-mode
                      ;; https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes
                      groovy-mode

                      ;; racket-mode
                      ;; https://github.com/greghendershott/racket-mode
                      racket-mode

                      ;; interleave
                      ;; https://github.com/rudolfochrist/interleave
                      interleave

                      ;; org-ref
                      ;; https://github.com/jkitchin/org-ref
                      org-ref

                      ;; protobuf-mode
                      ;; https://github.com/google/protobuf/blob/master/editors/protobuf-mode.el
                      protobuf-mode)

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

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;;;;
; Load mu4e
;;;;
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")

;;;;
;; Load grapviz and plantuml libraries
;;;;

(load-library "graphviz-dot-mode")
(load-library "plantuml-mode")

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

(load "mail.el")

(load "conf-autocomplete.el")
(load "conf-spellcheck.el")
(load "conf-org.el")
(load "conf-apib.el")
(load "conf-dired.el")
;;(load "conf-erc.el")

;; language specific
;; (load "lang-clojure.el")
(load "lang-scala.el")
;;(load "lang-c.el")
(load "lang-markdown.el")
(load "lang-go.el")
;;(load "lang-nzsql.el")

(load "ledger.el")

;;;;
;; Platform specific configurations
;;;;

;; Windows OS specific settings
(when (string-equal system-type "windows-nt")
  )

;; Mac OS X specific settings
(when (string-equal system-type "darwin")
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(org-export-backends (quote (ascii html icalendar latex odt confluence)))
 '(package-selected-packages
   (quote
    (protobuf-mode org-ref interleave yaml-mode web-mode terraform-mode solarized-theme smex rust-mode restclient rainbow-delimiters racket-mode projectile powerline perspective org ob-http magit ido-ubiquitous helm haskell-mode groovy-mode grizzl go-eldoc flycheck-ledger exec-path-from-shell ensime dockerfile-mode dklrt company-quickhelp clojure-mode-extra-font-locking clj-refactor autopair apib-mode ace-window)))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (put
            (quote call-with-values)
            (quote scheme-indent-function)
            1)
           (put
            (quote package)
            (quote scheme-indent-function)
            1)
           (put
            (quote struct)
            (quote scheme-indent-function)
            0)
           (put
            (quote with-exception-handler)
            (quote scheme-indent-function)
            0)
           (put
            (quote call-with-mutex)
            (quote scheme-indent-function)
            1)
           (put
            (quote call-with-context)
            (quote scheme-indent-function)
            1)
           (put
            (quote with-transaction)
            (quote scheme-indent-function)
            1)
           (put
            (quote call-with-transaction*)
            (quote scheme-indent-function)
            3)
           (put
            (quote call-with-transaction)
            (quote scheme-indent-function)
            3)
           (put
            (quote call-with-references-cursor)
            (quote scheme-indent-function)
            1)
           (put
            (quote call-with-history-cursor)
            (quote scheme-indent-function)
            1)
           (put
            (quote call-with-table-cursor)
            (quote scheme-indent-function)
            1)
           (put
            (quote call-with-index-cursor)
            (quote scheme-indent-function)
            1)
           (put
            (quote call-with-store-cursor)
            (quote scheme-indent-function)
            1)
           (put
            (quote call-with-store-index-cursor)
            (quote scheme-indent-function)
            1)
           (put
            (quote call-with-subjects-cursor)
            (quote scheme-indent-function)
            1)
           (put
            (quote call-with-predicates-cursor)
            (quote scheme-indent-function)
            1)
           (put
            (quote call-with-graph-cursor)
            (quote scheme-indent-function)
            1)
           (put
            (quote call-with-merge)
            (quote scheme-indent-function)
            3)
           (put
            (quote call-with-merge*)
            (quote scheme-indent-function)
            3)
           (put
            (quote run*)
            (quote scheme-indent-function)
            1)
           (put
            (quote fresh)
            (quote scheme-indent-function)
            1)
           (put
            (quote conde)
            (quote scheme-indent-function)
            nil)
           (put
            (quote with-cursor)
            (quote scheme-indent-function)
            1)
           (put
            (quote with-directory)
            (quote scheme-indent-function)
            1)
           (put
            (quote with-env)
            (quote scheme-indent-function)
            1)
           (put
            (quote with-context)
            (quote scheme-indent-function)
            1)
           (put
            (quote match)
            (quote scheme-indent-function)
            1)
           (put
            (quote with-transaction)
            (quote scheme-indent-function)
            1)
           (put
            (quote test-check)
            (quote scheme-indent-function)
            1)
           (put
            (quote call-with-cursor)
            (quote scheme-indent-function)
            1)
           (put
            (quote with-cnx)
            (quote scheme-indent-function)
            1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#000053f06969"))))
 '(company-scrollbar-fg ((t (:background "#00003f8d4fcf"))))
 '(company-tooltip ((t (:inherit default :background "#000033524073"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))
(put 'dired-find-alternate-file 'disabled nil)
