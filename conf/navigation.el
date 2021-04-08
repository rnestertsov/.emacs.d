;; navigation.el

;; setup projectile
(projectile-global-mode)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)
(setq projectile-create-missing-test-files t)

(require 'amx)
(amx-mode 1)

;;
;; (all-the-icons-ivy-setup)

;; ivy
;; make ivy nicer
;; (require 'ivy-rich)
;; (ivy-rich-mode 1)
;; (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x d") 'counsel-dired)
(global-set-key (kbd "C-c s") 'window-swap-states)
(setq ivy-rich-path-style 'abbrev)

;; (define-key ivy-minibuffer-map (kbd "C-<SPC>") 'ivy-mark)
;; (define-key ivy-minibuffer-map (kbd "S-<SPC>") 'ivy-unmark)

;;
;; Dired sidebar configuration
;;
(defun my/toggle-dired-sidebar ()
  "Toggle or navigate to dired sidebar."
  (interactive)
  (if (not (fboundp 'dired-sidebar-buffer))
      (progn
        (dired-sidebar-show-sidebar)
        (dired-sidebar-jump-to-sidebar))
    (if (eq (dired-sidebar-buffer) (window-buffer (selected-window))) ;; check if sidebar is focused
        (dired-sidebar-hide-sidebar)
      (if (dired-sidebar-showing-sidebar-p)
          (dired-sidebar-jump-to-sidebar)
        (progn
          (dired-sidebar-show-sidebar)
          (dired-sidebar-jump-to-sidebar))))))

(global-set-key (kbd "C-x C-n") 'my/toggle-dired-sidebar)

;; (setq dired-sidebar-should-follow-file 't)
;; (setq dired-sidebar-follow-file-idle-delay 1)

;; counsel-projectile
(require 'counsel-projectile)
(counsel-projectile-mode 1)

(defun my/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(global-set-key (kbd "M-[") 'my/switch-to-last-buffer)

;; ace-window configuration
;; (global-set-key (kbd "C-c o") 'ace-window)
;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;;;
;; projectile
;;;;
(defun my/projectile-run-vterm-in-root ()
  "Invoke `vterm` in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (vterm-other-window)))

;; TODO(roman): implement toggle behavior
(define-key projectile-mode-map (kbd "C-c p t") 'my/projectile-run-vterm-in-root)

(setq projectile-switch-project-action #'projectile-dired)

(add-to-list 'projectile-globally-ignored-directories "*node_modules")
(add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")
(add-to-list 'projectile-globally-ignored-directories "dist")
(add-to-list 'projectile-globally-ignored-directories "build")
(add-to-list 'projectile-globally-ignored-directories "CMakeFiles")
(add-to-list 'projectile-globally-ignored-directories "*target")

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p s g") 'rg-project)
(define-key projectile-mode-map (kbd "C-c p x c") 'projectile-run-command-in-root)

;; workaround to fix existing issue
(setq projectile-project-run-cmd "")

;; TODO: implement following behavior
;; - if Makefile is found -> exec "make build" command
;; - if build.sh if found -> exec "build.sh" command
;; - if nothing is found and we are in go-mode -> build current file
;; - otherwise -> ask for build command
(defun my/projectile-compile-project (&optional prompt)
  (interactive "P")
  (let ((compilation-read-command
         (or (not (projectile-compilation-command (projectile-compilation-dir)))
             prompt)))
    (projectile-compile-project prompt)))

;; TODO: implement following behavior
;; - if Makefile is found -> exec "make run" command
;; - if build.sh if found -> exec "run.sh" command
;; - if nothing is found and we are in go-mode -> run current file
;; - otherwise -> as for run command
(defun my/projectile-run-project (&optional prompt)
  (interactive "P")
  (let ((compilation-read-command
         (or (not (projectile-run-command (projectile-compilation-dir)))
             prompt)))
    (projectile-run-project prompt)))

;; TODO: ignore dired-sidebar when switching between windows
;; https://stackoverflow.com/questions/4941960/how-do-i-make-emacs-other-window-command-ignore-terminal-windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(windmove-default-keybindings 'meta)

(require 'dired)
;; Rebind RET from dired-advertised-find-file to
;; dired-find-alternate-file so that we don't have dired buffer bloat.
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; Rebind ^ to use find-alternate-file to go up a directory, again to
;; prevent dired buffer bloat.
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
