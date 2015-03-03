;; navigation.el

;; setup projectile
(projectile-global-mode)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)

;; Enhances M-x to allow easier navigation.
;; Provides autocompletion
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; The forward naming method includes part of the directory name
;; at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_code/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can switch
;; to recently edited files
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;;;;
;; ido
;;;;

;; use ido-mode for easy navigation across buffers
(ido-mode 1)

;; enable partial matching
(setq ido-enable-flex-matching t)

(setq ido-use-filename-at-point nil)

;; match files only in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; includes buffer names of recently open files, even if they are
;; not open now
(setq ido-use-virtual-buffers t)

;; enable ido in all contexts where it might be usefull
(ido-ubiquitous-mode 1)

;; vi-like navigation
(defhydra vi-like-nav (global-map "<f12>")
  "vi-like-nav"
  ("l" forward-char "right")
  ("h" backward-char "left")
  ("j" next-line "down")
  ("k" previous-line "up")
  ("q" nil "cancel"))

;; ace-window configuration
(global-set-key (kbd "M-p") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))


