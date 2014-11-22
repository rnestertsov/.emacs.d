;;; emacs-conf-autocomplete.el ---

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/ac-dict")
(ac-config-default)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(setq ac-sources (delq 'ac-source-yasnippet ac-sources))
;(define-key ac-completing-map "\M-/" 'ac-stop)
                                        ; use M-/ to stop
                              ; completion

