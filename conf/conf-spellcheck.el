;;; spellcheck.el ---

;; configure aspell spell checker

(setq-default ispell-program-name "aspell.exe")
(flyspell-mode-off)
;; (setq text-mode-hook '(lambda ()
                        ;; (flyspell-mode nil)))
