;; misc.el

;; change all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; no need for ~ files
(setq create-lockfiles nil)

;; open *scratch* buffer at startup
(setq inhibit-startup-message t)
