;;; emacs-conf-autocomplete.el ---

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)

(global-set-key [(control tab)] 'company-complete-common)
(setq company-idle-delay 0.5)

(company-quickhelp-mode 1)

;; configure colors
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

