;;; ui.el ---

(delete-other-windows)
(split-window-horizontally)

;; smooth-scrolling stops that annoying jump when moving around
(require 'smooth-scrolling)
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 3
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; prevent emacs from exiting without asking user
(setq confirm-kill-emacs 'y-or-n-p)

;; disable menu bar
(menu-bar-mode -1)

;; disable toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; set standard keys for copy and paste
(cua-mode 1)

;; don't show native OS scroll bars
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; do not show line numbers
(global-linum-mode 0)

;;;;
;; Themes
;;;;

;; read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; use inconsolata / 120 as default font
;; or consolas 110
(set-face-attribute 'default nil
                    :family "Consolas"
                    :height 140)

;; (set-face-attribute 'default nil
                    ;; :family "monospace"
                    ;; :height 140)

;; make titlebar same colo as Emacs background
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; no cursor blinking
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; no bell
(setq ring-bell-function 'ignore)

;; set color theme
;; (load-theme 'solarized-dark t)
(load-theme 'cyberpunk t)

;;
;; configure side windows
;;
(defvar parameters
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t)
                         (mode-line-format . none))))

(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)

(setq compilation-scroll-output 'first-error)

(setq display-buffer-alist
      `(("\\*\\(compilation\\|Go Test\\|Gofmt Errors\\)\\*" display-buffer-in-side-window
         (side . bottom) (slot . 1) (preserve-size . (nil . t))
         ,parameters)))
