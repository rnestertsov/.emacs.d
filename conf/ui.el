;;; ui.el ---

;; smooth-scrolling stops that annoying jump when moving around
(require 'smooth-scrolling)
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

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
                    :height 150)

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
(load-theme 'solarized-dark t)

;; powerline
;; (require 'powerline)
;; (powerline-default-theme)

;; (setq powerline-color1 "#073642")
;; (setq powerline-color2 "#002b36")

;; (set-face-attribute 'mode-line nil
                    ;; :foreground "#fdf6e3"
                    ;; :background "#2aa198"
                    ;; :box nil
                    ;; :inverse-video nil)
;; (set-face-attribute 'mode-line-inactive nil
                    ;; :box nil
                    ;; :inverse-video nil)
