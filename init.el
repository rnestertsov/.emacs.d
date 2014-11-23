;;; init.el ---

;;;;
;; Packages
;;;;

;; define package repositories
(require 'package)
;;(add-to-list 'package-archives
;;             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives
;;             '("org" . "http://orgmode.org/elpa/") t)

;; load and activate emacs packages.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(go-mode
                      clojure-test-mode
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
;; ENVIRONMENT
;;;;

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shell-PATH))

(setenv "PATH" (concat (getenv "PATH") ":/bin"))
(setq exec-path (append exec-path '("/bin")))

;; Windows specific paths
(when (string-equal system-type "windows-nt")
      (progn
        ;;
        (setenv "PATH"
                (concat
                 "C:/bin/" ";"
                 "C:/Program Files/Java/jdk1.7.0_51/bin/" ";"
                 "C:/bin/graphviz/bin/" ";"))
        (setq exec-path
              '(
                "C:/bin/"
                "C:/Program Files/Java/jdk1.7.0_51/bin/"
                "C:/bin/graphviz/bin/"))
        ))

;;--------------------------------------------------

(defun osxp ()
  (string= "darwin" system-type))

(when (and (osxp) window-system)
  ;; make option the super key on mac
  (setq mac-option-modifier 'super)
  ;; map meta to command key for mac
  (setq ns-command-modifier 'meta))

(load-library "graphviz-dot-mode")
(load-library "plantuml-mode")

;;;;
;; Configuration
;;;;

(add-to-list 'load-path "~/.emacs.d/conf")

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
