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

;; show line numbers
(global-linum-mode t)


;;;;
;; Themes
;;;;

;; read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html for tech details
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
;;(load-theme 'tomorrow-night-bright t)

;; use inconsolata / 120 as default font
;; or consolas 110
(set-face-attribute 'default nil
                    :family "Consolas"
                    :height 110)

;; no cursor blinking
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; no bell
(setq ring-bell-function 'ignore)

;; set color theme
;; it's based on the time
(setq current-theme '(color-theme-solarized-light))

(defun set-current-theme ()
  (setq hour (string-to-number (substring (current-time-string) 11 13)))
  (if (member hour (number-sequence 6 17))
    (setq now '(color-theme-solarized-light))
    (setq now '(color-theme-solarized-dark)))
  (if (eq now current-theme)
    nil
    (setq current-theme now)
    (eval now)))

(run-with-timer 0 3600 'set-current-theme)

(set-current-theme)

;; solarized
;; (when window-system
  ;; (require 'color-theme)
  ;; (eval-after-load 'color-theme
    ;; (progn
      ;; (color-theme-solarized-dark))))

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
