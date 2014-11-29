;;; spellcheck.el ---

;; configure aspell spell checker

(setq-default ispell-program-name "aspell.exe")
(setq text-mode-hook '(lambda ()
                        (flyspell-mode nil)))
