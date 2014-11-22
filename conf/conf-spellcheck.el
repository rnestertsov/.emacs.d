;;; spellcheck.el ---

;; configure aspell spell checker

(setq-default ispell-program-name "C:/bin/Aspell/bin/aspell.exe")
(setq text-mode-hook '(lambda ()
                        (flyspell-mode t)))
