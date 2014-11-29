;;; init.el ---

;;;;
;; Packages
;;;;

;; define package repositories
(require 'package)
(add-to-list 'package-archives
            '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
            '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("marmelade" . "http://marmalade-repo.org/packages/") t)

;; load and activate emacs packages.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(go-mode
                      clojurescript-mode
                      color-theme
                      color-theme-solarized
                      cider
                      ac-cider
                      auto-complete
                      popup
                      rainbow-delimiters
                      markdown-mode
                      yasnippet
                      desktop
                      autopair
                      grizzl
                      projectile
                      perspective
                      magit
                      org
                      powerline)

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
;;(load "lang-nzsql.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("c:/Users/rnestertsov/My Box Files/Roman Nestertsov/org/work.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
