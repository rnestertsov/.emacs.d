;; misc.el

;; change all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; no need for ~ files
(setq create-lockfiles nil)

;; open *scratch* buffer at startup
;; (setq inhibit-startup-message t)

;; open *bookmarks* buffer at startup
(setq inhibit-splash-screen t)
(require 'bookmark)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")
(toggle-frame-maximized)

(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name `persistent-scratch-file-name'."
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max) "~/.emacs.d/persistent-scratch")))

(defun load-persistent-scratch ()
  "Load the contents of `persistent-scratch-file-name' into the scratch buffer, clearing its contents first."
  (if (file-exists-p "~/.emacs-persistent-scratch")
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
        (insert-file-contents "~/.emacs.d/persistent-scratch"))))

(add-hook 'after-init-hook 'load-persistent-scratch)
(add-hook 'kill-emacs-hook 'save-persistent-scratch)

(if (not (boundp 'cce/save-persistent-scratch-timer))
    (setq cce/save-persistent-scratch-timer
          (run-with-idle-timer 300 t 'save-persistent-scratch)))

;; term
;; (setq locale-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8-unix)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)
;; (ansi-color-for-comint-mode-on)
;; (setq system-uses-terminfo nil)
(add-hook 'term-mode-hook (lambda()
        (setq yas-dont-activate t)))

;; ensure files have no trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; configure column wrapping
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
          '(lambda()
             (set-fill-column 120)))
