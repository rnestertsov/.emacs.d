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
